package org.ergoplatform.http.routes

import akka.actor.{Actor, Props}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.Json
import io.circe.syntax._
import org.ergoplatform.http.api.{ApiCodecs, TransactionsApiRoute}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{ErgoStateContext, VotingData}
import org.ergoplatform.settings.Constants.TrueTree
import org.ergoplatform.settings.{Args, ErgoSettings, ErgoSettingsReader, NetworkType, YoloLaunchParameters}
import org.ergoplatform.utils.StorageRentTestHelpers
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, ErgoTreePredef, Input}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.util.bytesToId
import sigma.Colls
import sigma.data.Digest32Coll
import sigma.interpreter.ProverResult

import java.io.File
import java.nio.ByteBuffer
import scala.concurrent.duration._

/**
  * The YOLO fee output policy (`YoloFeeOutputPolicy`) at the API's transaction check, POST /transactions/check
  * (`ErgoBaseApiRoute.verifyTransaction`, shared with POST /transactions and the wallet's send routes), against a
  * UTXO state with the YOLO launch parameters: with the switch on, a transaction with many fee outputs (9 inputs, 730 fee outputs of
  * 0.01) and a fee output carrying a token are refused with the policy's reason; with the switch off both pass, as
  * upstream.
  */
class YoloFeeOutputPolicyRouteSpec extends AnyFlatSpec
  with Matchers
  with ScalatestRouteTest
  with StorageRentTestHelpers
  with ApiCodecs
  with FailFastCirceSupport {

  import org.ergoplatform.utils.ErgoCoreTestConstants.{genesisStateDigest, validationSettingsNoIl}
  import org.ergoplatform.utils.generators.ErgoCoreGenerators.defaultHeaderGen

  implicit val routeTimeout: RouteTestTimeout = RouteTestTimeout(60.seconds)

  private lazy val devnetPath: String =
    new File(getClass.getClassLoader.getResource("yolo-devnet.conf").toURI).getAbsolutePath
  private lazy val devnet: ErgoSettings = ErgoSettingsReader
    .read(Args(Some(devnetPath), Some(NetworkType.TestNet)))
    .copy(directory = createTempDir.getAbsolutePath)
  private lazy val switchOff: ErgoSettings = devnet.copy(chainSettings = devnet.chainSettings.copy(
    yolo = devnet.chainSettings.yolo.copy(enabled = false)))

  private val tip: Int = StoragePeriod + 99
  private val next: Int = tip + 1
  private val Coin: Long = 1000000000L
  private val Fee: Long = Coin / 100
  private lazy val feeProp = ErgoTreePredef.feeProposition(devnet.chainSettings.monetary.minerRewardDelay)

  private def id(prefix: Int, i: Int): Array[Byte] = ByteBuffer.allocate(32).putInt(prefix).putInt(i).array

  private lazy val inputs: IndexedSeq[ErgoBox] = (0 until 9).map { i =>
    new ErgoBoxCandidate(Coin, TrueTree, tip - 10).toBox(bytesToId(id(0x5a5a5a5a, i)), 0)
  }

  private lazy val tokenBox: ErgoBox = new ErgoBox(Coin, TrueTree,
    Colls.fromItems((Digest32Coll @@ Colls.fromArray(id(0x5b5b5b5b, 0))) -> 10L), Map.empty,
    bytesToId(id(0x5c5c5c5c, 0)), 0, tip - 10)

  /** A transaction with many fee outputs: the 9 inputs into a change output and 730 fee outputs of 0.01 */
  private lazy val probe: ErgoTransaction = {
    val k = 730
    ErgoTransaction(inputs.map(b => new Input(b.id, ProverResult.empty)), IndexedSeq.empty,
      (new ErgoBoxCandidate(inputs.map(_.value).sum - k * Fee, TrueTree, next) +:
        Seq.fill(k)(new ErgoBoxCandidate(Fee, feeProp, next))).toIndexedSeq)
  }

  /** A fee output carrying the token box's token */
  private lazy val tokenFee: ErgoTransaction =
    ErgoTransaction(IndexedSeq(new Input(tokenBox.id, ProverResult.empty)), IndexedSeq.empty, IndexedSeq(
      new ErgoBoxCandidate(Coin - Fee, TrueTree, next),
      new ErgoBoxCandidate(Fee, feeProp, next, tokenBox.additionalTokens)))

  private def ctxAtTip(s: ErgoSettings): ErgoStateContext = {
    val header = defaultHeaderGen.sample.get.copy(version = YoloLaunchParameters.blockVersion, height = tip)
    new ErgoStateContext(Seq(header), None, genesisStateDigest, YoloLaunchParameters, validationSettingsNoIl,
      VotingData.empty)(s.chainSettings)
  }

  /** The transactions routes over a UTXO state at `tip` holding the inputs, with an empty mempool */
  private def routeFor(s: ErgoSettings): Route = {
    val state = utxoStateAt(inputs :+ tokenBox, None, ctxAtTip(s), s)
    val pool = ErgoMemPool.empty(s)
    val readers = system.actorOf(Props(new Actor {
      override def receive: Receive = { case GetReaders => sender() ! Readers(null, state, pool, null) }
    }))
    val nodeView = system.actorOf(Props(new Actor {
      override def receive: Receive = { case _ => }
    }))
    TransactionsApiRoute(readers, nodeView, s).route
  }

  it should "refuse a tx with 730 fee outputs and a token fee output at /transactions/check with the switch on" in {
    val route = routeFor(devnet)
    probe.size should be < devnet.nodeSettings.maxTransactionSize
    Post("/transactions/check", probe.asJson) ~> route ~> check {
      status shouldBe StatusCodes.BadRequest
      responseAs[Json].noSpaces should include(
        s"its 730 fee output(s) add up to ${730 * 2100} to the miner's fee transaction")
    }
    Post("/transactions/check", tokenFee.asJson) ~> route ~> check {
      status shouldBe StatusCodes.BadRequest
      responseAs[Json].noSpaces should include("fee output #1 carries 1 token(s)")
    }
  }

  it should "pass both at /transactions/check with the switch off, as upstream" in {
    val route = routeFor(switchOff)
    Post("/transactions/check", probe.asJson) ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[String] shouldEqual probe.id
    }
    Post("/transactions/check", tokenFee.asJson) ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[String] shouldEqual tokenFee.id
    }
  }

}
