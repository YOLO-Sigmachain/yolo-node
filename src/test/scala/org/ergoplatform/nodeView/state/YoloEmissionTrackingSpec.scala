package org.ergoplatform.nodeView.state

import org.ergoplatform.ErgoBox.{R4, R5}
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import org.ergoplatform.mining.emission.EmissionRules
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.BlockTransactions
import org.ergoplatform.modifiers.history.extension.Extension
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.settings.{Args, ErgoSettings, ErgoSettingsReader, NetworkType, YoloEmissionSettings,
  YoloLaunchParameters}
import org.ergoplatform.settings.Constants.TrueTree
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.wallet.utils.TestFileUtils
import sigma.Colls
import sigma.ast.ErgoTree
import sigma.data.Digest32Coll
import sigma.interpreter.ProverResult

import java.io.File

/**
  * YOLO emission box tracking and the full-search spam filter, `UtxoStateReader.extractEmissionBox`, and
  * the stored emission box id after the terminal spend.
  */
class YoloEmissionTrackingSpec extends ErgoCorePropertyTest with TestFileUtils {
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.ErgoCoreTestConstants.parameters
  import org.ergoplatform.utils.generators.ErgoCoreGenerators._

  private lazy val devnetPath: String =
    new File(getClass.getClassLoader.getResource("yolo-devnet.conf").toURI).getAbsolutePath
  // the full, checked read: exits the JVM if the devnet conf fails a start-up check
  private lazy val devnet: ErgoSettings = ErgoSettingsReader
    .read(Args(Some(devnetPath), Some(NetworkType.TestNet)))
    .copy(directory = createTempDir.getAbsolutePath) // states open a snapshots db under it
  private lazy val e: YoloEmissionSettings = devnet.chainSettings.yolo.emission
  private lazy val boxes: Seq[ErgoBox] = ErgoState.genesisBoxes(devnet.chainSettings)
  private lazy val emissionBox: ErgoBox = boxes.head

  private def token(id: Array[Byte], amount: Long = 1L) = (Digest32Coll @@ Colls.fromArray(id)) -> amount

  private def state(s: ErgoSettings, genesis: Seq[ErgoBox], tracked: Option[ErgoBox]): UtxoState =
    UtxoState.fromBoxHolder(BoxHolder(genesis), tracked, createTempDir, s,
      if (s.chainSettings.yolo.enabled) YoloLaunchParameters else parameters)

  private def block(txs: ErgoTransaction*): ErgoFullBlock = {
    val header = defaultHeaderGen.sample.get.copy(height = 1)
    ErgoFullBlock(header, BlockTransactions(header.id, header.version, txs), Extension(header.id, Seq.empty), None)
  }

  private def spend(inputs: Seq[ErgoBox], outputs: ErgoBoxCandidate*): ErgoTransaction =
    ErgoTransaction(inputs.map(b => new Input(b.id, ProverResult.empty)).toIndexedSeq, outputs.toIndexedSeq)

  private def candidate(value: Long,
                        tree: ErgoTree,
                        tokens: Seq[(ErgoBox.TokenId, Long)] = Seq.empty,
                        regs: ErgoBox.AdditionalRegisters = Map.empty): ErgoBoxCandidate =
    new ErgoBoxCandidate(value, tree, 1, Colls.fromItems(tokens: _*), regs)

  private lazy val regs: ErgoBox.AdditionalRegisters =
    Map(R4 -> emissionBox.additionalRegisters(R4), R5 -> emissionBox.additionalRegisters(R5))
  private val Reward = 75000000000L

  // a normal-path coinbase layout (scripts are not run by extractEmissionBox)
  private def successor(value: Long, tokens: Seq[(ErgoBox.TokenId, Long)]): ErgoBoxCandidate =
    candidate(value, e.emissionTree, tokens, regs)
  private def shares: Seq[ErgoBoxCandidate] = Seq(
    candidate(3000000000L, e.treasuryAccumulationTree),
    candidate(2250000000L, e.lpAccumulationTree),
    candidate(69750000000L, TrueTree))

  private def otherTx: ErgoTransaction = spend(Seq(boxes(3)), candidate(1000000000L, TrueTree))

  property("the genesis emission box is recorded and is the YOLO emission box") {
    val us = state(devnet, boxes, boxes.headOption)
    us.emissionBoxOpt shouldBe Some(emissionBox)
    e.isEmissionBox(emissionBox) shouldBe true
    us.closeStorage()
  }

  property("tracked box spent: the real successor (emission tree, tokens(0) = (NFT, 1)) is tracked") {
    val us = state(devnet, boxes, boxes.headOption)
    val tx = spend(Seq(emissionBox), successor(emissionBox.value - Reward, Seq(token(e.emissionNftIdBytes))) +: shares: _*)
    us.extractEmissionBox(block(otherTx, tx)) shouldBe Some(tx.outputs.head)
    // not spent in this block: the tracked box stays
    us.extractEmissionBox(block(otherTx)) shouldBe Some(emissionBox)
    us.closeStorage()
  }

  property("tracked box spent into a lookalike (emission tree, wrong token or none): not tracked") {
    val us = state(devnet, boxes, boxes.headOption)
    val lookalikes = Seq(
      "other token" -> Seq(token(e.treasuryNftIdBytes)),
      "no token" -> Seq.empty,
      "NFT amount 2" -> Seq(token(e.emissionNftIdBytes, 2L)),
      "NFT not first" -> Seq(token(e.lpNftIdBytes), token(e.emissionNftIdBytes)))
    lookalikes.foreach { case (name, tokens) =>
      withClue(s"$name: ") {
        val tx = spend(Seq(emissionBox), successor(emissionBox.value - Reward, tokens) +: shares: _*)
        e.isEmissionBox(tx.outputs.head) shouldBe false
        us.extractEmissionBox(block(tx)) shouldBe None
      }
    }
    us.closeStorage()
  }

  property("terminal spend (NFT burned, no output at the emission tree): tracking returns None") {
    val last = ErgoState.createGenesisBox(1500000000L, e.emissionTree, Seq(token(e.emissionNftIdBytes)), regs)
    val genesis = last +: boxes.tail
    val us = state(devnet, genesis, Some(last))
    us.emissionBoxOpt shouldBe Some(last)
    val terminal = spend(Seq(last),
      candidate(60000000L, e.treasuryAccumulationTree),
      candidate(45000000L, e.lpAccumulationTree),
      candidate(1395000000L, TrueTree))
    us.extractEmissionBox(block(terminal)) shouldBe None
    us.closeStorage()
  }

  property("full search (no tracked box): only a box at the emission tree with the NFT is found, of any value") {
    val us = state(devnet, boxes, None)
    def found(tokens: Seq[(ErgoBox.TokenId, Long)], value: Long, tree: ErgoTree = e.emissionTree): Option[ErgoBox] =
      us.extractEmissionBox(block(otherTx, spend(Seq(boxes(1)), candidate(value, tree, tokens, regs))))
    // lookalikes at the emission script are not found
    found(Seq(token(e.lpNftIdBytes)), 177291450000000000L) shouldBe None
    found(Seq.empty, 177291450000000000L) shouldBe None
    found(Seq(token(e.emissionNftIdBytes, 2L)), 177291450000000000L) shouldBe None
    // the NFT at another script is not found
    found(Seq(token(e.emissionNftIdBytes)), 177291450000000000L, TrueTree) shouldBe None
    // the real box is found even below Ergo's 100,000-coin spam filter (the last YOLO blocks)
    val small = found(Seq(token(e.emissionNftIdBytes)), 1500000000L)
    small.map(_.value) shouldBe Some(1500000000L)
    small.exists(e.isEmissionBox) shouldBe true
    found(Seq(token(e.emissionNftIdBytes)), 99999L * EmissionRules.CoinsInOneErgo).map(_.value) shouldBe
      Some(99999L * EmissionRules.CoinsInOneErgo)
    us.closeStorage()
  }

  property("switch off: upstream tracking by the Ergo emission tree, with the 100,000-coin filter") {
    val us = state(settings, genesisBoxes, None)
    val prop = settings.chainSettings.monetary.emissionBoxProposition
    def found(value: Long): Option[ErgoBox] =
      us.extractEmissionBox(block(spend(Seq(genesisBoxes(1)), candidate(value, prop))))
    found(100000L * EmissionRules.CoinsInOneErgo) shouldBe None
    found(100000L * EmissionRules.CoinsInOneErgo + 1).map(_.value) shouldBe Some(100000L * EmissionRules.CoinsInOneErgo + 1)
    // the YOLO NFT rule is not applied: no token needed
    settings.chainSettings.yolo.enabled shouldBe false
    us.closeStorage()
  }

}
