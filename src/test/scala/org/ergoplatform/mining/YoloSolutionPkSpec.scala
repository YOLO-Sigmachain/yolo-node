package org.ergoplatform.mining

import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.StatusReply
import akka.testkit.{TestKit, TestProbe}
import org.ergoplatform.mining.CandidateGenerator.{Candidate, GenerateCandidate}
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.nodeView.{ErgoNodeViewRef, ErgoReadersHolderRef, LocallyGeneratedModifier}
import org.ergoplatform.settings.{Args, ErgoSettings, ErgoSettingsReader, NetworkType}
import org.ergoplatform.utils.ErgoTestHelpers
import org.ergoplatform.utils.generators.ErgoCoreGenerators.genECPoint
import org.ergoplatform.wallet.utils.TestFileUtils
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sigma.crypto.{CryptoConstants, CryptoFacade}
import sigma.data.ProveDlog
import sigmastate.crypto.DLogProtocol.DLogProverInput

import java.io.File
import scala.concurrent.duration._

/**
  * An external solution posted without a key (the identity point): with the YOLO switch on, the block gets the key the
  * chosen candidate was built for (the YOLO coinbase pays that key); with the switch off, the node's key, as upstream.
  */
class YoloSolutionPkSpec extends AnyFlatSpec with Matchers with ErgoTestHelpers with TestFileUtils {
  import org.ergoplatform.utils.ErgoCoreTestConstants.defaultMinerSecret

  private val candidateGenDelay: FiniteDuration = 10.seconds
  private val blockValidationDelay: FiniteDuration = 5.seconds

  // the key a candidate is built for through /mining/candidateWithTxsAndPk, not the node's key
  private val otherPk: ProveDlog = DLogProverInput(BigInt("1234567890123456789012345678901234567890").bigInteger).publicImage

  /** `s` as a mining node in a temp directory, with fake PoW (any solution validates). */
  private def miningNode(s: ErgoSettings, divisor: Int): ErgoSettings = {
    val dir = createTempDir.getAbsolutePath
    s.copy(
      directory = dir,
      nodeSettings = s.nodeSettings.copy(mining = true, stateType = StateType.Utxo, offlineGeneration = true,
        verifyTransactions = true, internalMinerPollingInterval = 1.second),
      chainSettings = s.chainSettings.copy(powScheme = new DefaultFakePowScheme(32, 26, divisor)),
      walletSettings = s.walletSettings.copy(secretStorage = s.walletSettings.secretStorage.copy(secretDir = dir + "/keystore")))
  }

  private lazy val devnetPath: String =
    new File(getClass.getClassLoader.getResource("yolo-devnet.conf").toURI).getAbsolutePath

  /** Posts a solution without a key on a candidate built for `otherPk`; returns the key of the block header. */
  private def keyOfSolvedBlock(s: ErgoSettings)(implicit system: ActorSystem): ProveDlog = {
    val replyProbe = new TestProbe(system)
    val viewHolderProbe = new TestProbe(system)
    val realViewHolderRef: ActorRef = ErgoNodeViewRef(s)
    val readersHolderRef: ActorRef = ErgoReadersHolderRef(realViewHolderRef)
    val candidateGenerator: ActorRef =
      CandidateGenerator(defaultMinerSecret.publicImage, readersHolderRef, viewHolderProbe.ref, s)

    candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = false, Some(otherPk)), replyProbe.ref)
    val candidate = replyProbe.expectMsgPF(candidateGenDelay) { case StatusReply.Success(c: Candidate) => c }
    candidate.externalVersion.pk shouldBe otherPk

    val identity = CryptoConstants.dlogGroup.identity
    CryptoFacade.isInfinityPoint(identity) shouldBe true
    candidateGenerator.tell(AutolykosSolution(identity, genECPoint.sample.get, Array.fill(8)(0: Byte), BigInt(0)),
      replyProbe.ref)
    replyProbe.expectMsg(blockValidationDelay, StatusReply.Success(()))
    val header = viewHolderProbe.expectMsgPF(blockValidationDelay) { case LocallyGeneratedModifier(h: Header) => h }
    ProveDlog(header.powSolution.pk)
  }

  it should "give a keyless solution the chosen candidate's own key with the YOLO switch on" in new TestKit(ActorSystem()) {
    val yolo = miningNode(ErgoSettingsReader.read(Args(Some(devnetPath), Some(NetworkType.TestNet))), divisor = 4)
    yolo.chainSettings.yolo.enabled shouldBe true
    keyOfSolvedBlock(yolo)(system) shouldBe otherPk
    system.terminate()
  }

  it should "give a keyless solution the node's key with the YOLO switch off, as upstream" in new TestKit(ActorSystem()) {
    val ergo = miningNode(ErgoSettingsReader.read(), divisor = 1)
    ergo.chainSettings.yolo.enabled shouldBe false
    keyOfSolvedBlock(ergo)(system) shouldBe defaultMinerSecret.publicImage
    system.terminate()
  }

}
