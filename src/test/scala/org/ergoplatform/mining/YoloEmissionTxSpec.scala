package org.ergoplatform.mining

import org.ergoplatform.ErgoBox.{R4, R5, R6}
import org.ergoplatform.mining.emission.YoloEmissionRules
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.popow.NipopowAlgos
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.ErgoContext
import org.ergoplatform.nodeView.state.{BoxHolder, ErgoState, ErgoStateContext, UtxoState}
import org.ergoplatform.sdk.wallet.protocol.context.TransactionContext
import org.ergoplatform.settings.{Args, ErgoSettings, ErgoSettingsReader, ErgoValidationSettingsUpdate, NetworkType,
  YoloEmissionSettings, YoloLaunchParameters, YoloSettings}
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.wallet.interpreter.ErgoInterpreter
import org.ergoplatform.wallet.protocol.context.InputContext
import org.ergoplatform.wallet.utils.TestFileUtils
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, ErgoTreePredef, Input, P2PKAddress}
import org.ergoplatform.settings.Constants.TrueTree
import sigma.interpreter.ProverResult
import scorex.util.bytesToId
import scorex.util.encode.Base16
import sigma.Colls
import sigma.ast.{ByteArrayConstant, ErgoTree}
import sigma.data.{Digest32Coll, ProveDlog}
import sigmastate.crypto.DLogProtocol.DLogProverInput

import java.io.File
import scala.util.Try

/**
  * YOLO coinbase builder (with the coinbase miner delay of 2880): the transactions `YoloEmissionTx` builds, reached
  * through `CandidateGenerator.collectEmission` / `collectRewards` with the switch on, are accepted by the real
  * compiled emission.es (the devnet tree) under the node's stateful transaction validation, and applied in full
  * blocks to a YOLO genesis UTXO state. Negative controls show the script is really evaluated: each mutation keeps
  * every non-script rule satisfied (ERG preserved, no dust, valid heights and assets) and is rejected by rule 119
  * (`txScriptValidation`).
  */
class YoloEmissionTxSpec extends ErgoCorePropertyTest with TestFileUtils {
  import org.ergoplatform.utils.ErgoCoreTestConstants.{defaultMinerPk, defaultMinerSecretNumber, defaultTimestamp,
    powScheme}
  import org.ergoplatform.utils.generators.ErgoCoreGenerators._

  private lazy val devnetPath: String =
    new File(getClass.getClassLoader.getResource("yolo-devnet.conf").toURI).getAbsolutePath
  // the full, checked read: exits the JVM if the devnet conf fails a start-up check
  private lazy val devnet: ErgoSettings = ErgoSettingsReader
    .read(Args(Some(devnetPath), Some(NetworkType.TestNet)))
    .copy(directory = createTempDir.getAbsolutePath) // states open a snapshots db under it
  private lazy val e: YoloEmissionSettings = devnet.chainSettings.yolo.emission

  private val Coin = YoloEmissionRules.CoinsInOneYolo
  // the devnet genesis emission value, as yolo-devnet.conf ships it
  private val GenesisValue = 177291250L * Coin
  private val ScriptRejected = "Scripts of all transaction inputs should pass verification"

  // a second miner key, for the wrong-key control and the custom-key path
  private val otherSecret: BigInt = BigInt("1234567890123456789012345678901234567890")
  private val otherPk: ProveDlog = DLogProverInput(otherSecret.bigInteger).publicImage

  private def withGenesisValue(v: Long): ErgoSettings = devnet.copy(chainSettings = devnet.chainSettings.copy(
    yolo = devnet.chainSettings.yolo.copy(emission = e.copy(genesisValue = v))))

  private def nft: (ErgoBox.TokenId, Long) = (Digest32Coll @@ Colls.fromArray(e.emissionNftIdBytes)) -> 1L

  /** A UTXO state holding `genesis`, tracking its first box as the emission box. */
  private def stateOf(s: ErgoSettings, genesis: Seq[ErgoBox]): UtxoState =
    UtxoState.fromBoxHolder(BoxHolder(genesis), genesis.headOption, createTempDir, s, YoloLaunchParameters)

  private def genesisState(s: ErgoSettings = devnet): UtxoState = stateOf(s, ErgoState.genesisBoxes(s.chainSettings))

  /** Context of the block after the last one in `sc`, mined by `pk`, as `CandidateGenerator.createCandidate` makes. */
  private def upcoming(sc: ErgoStateContext, pk: ProveDlog): ErgoStateContext =
    sc.upcoming(pk.value, defaultTimestamp, devnet.chainSettings.initialNBits, Array.fill(3)(0: Byte),
      ErgoValidationSettingsUpdate.empty, YoloLaunchParameters.blockVersion)

  /** `sc` with a (generated) last header at `height`, so that the upcoming block is at `height + 1`. */
  private def atHeight(sc: ErgoStateContext, height: Int): ErgoStateContext =
    new ErgoStateContext(Seq(defaultHeaderGen.sample.get.copy(height = height)), None, sc.genesisStateDigest,
      sc.currentParameters, sc.validationSettings, sc.votingData)(sc.chainSettings)

  /** The node's stateful validation of `tx` against `us` (the candidate path, `UtxoStateReader.validateWithCost`). */
  private def validate(us: UtxoState, tx: ErgoTransaction, ctx: ErgoStateContext): Try[Int] =
    us.validateWithCost(tx, ctx, ctx.currentParameters.maxBlockCost, None)

  private def rejectedByScript(us: UtxoState, tx: ErgoTransaction, ctx: ErgoStateContext): Unit = {
    val res = validate(us, tx, ctx)
    res.isFailure shouldBe true
    res.failed.get.getMessage should include(ScriptRejected)
  }

  /** Mines a block with `txs` on top of `us` (fake PoW, header key from `sk`) and applies it to the state. */
  private def mine(us: UtxoState,
                   parentOpt: Option[ErgoFullBlock],
                   txs: Seq[ErgoTransaction],
                   sk: BigInt = defaultMinerSecretNumber): (UtxoState, ErgoFullBlock) = {
    val (adProof, digest) = us.proofsForTransactions(txs).get
    // the state context validates interlinks with its own chain settings (real Autolykos difficulty)
    val popow = new NipopowAlgos(us.stateContext.chainSettings)
    val ext = popow.interlinksToExtension(popow.updateInterlinks(parentOpt.map(_.header), parentOpt.map(_.extension)))
    val time = parentOpt.map(_.header.timestamp + 1).getOrElse(defaultTimestamp)
    val fb = powScheme.proveBlock(parentOpt.map(_.header), YoloLaunchParameters.blockVersion,
      devnet.chainSettings.initialNBits, digest, adProof, txs, time, ext, Array.fill(3)(0: Byte), sk).get
    us.applyModifier(fb, None)(_ => ()).get -> fb
  }

  private def box(c: ErgoBoxCandidate,
                  value: Long = -1,
                  tree: sigma.ast.ErgoTree = null,
                  height: Int = -1,
                  tokens: Seq[(ErgoBox.TokenId, Long)] = null,
                  regs: ErgoBox.AdditionalRegisters = null): ErgoBoxCandidate =
    new ErgoBoxCandidate(
      if (value < 0) c.value else value,
      Option(tree).getOrElse(c.ergoTree),
      if (height < 0) c.creationHeight else height,
      Option(tokens).map(t => Colls.fromItems(t: _*)).getOrElse(c.additionalTokens),
      Option(regs).getOrElse(c.additionalRegisters))

  private def withOutputs(tx: ErgoTransaction)(outs: IndexedSeq[ErgoBoxCandidate]): ErgoTransaction =
    ErgoTransaction(tx.inputs, tx.dataInputs, outs)

  private def checkNormalLayout(tx: ErgoTransaction, input: ErgoBox, height: Int, pk: ProveDlog,
                                treasury: Long, lp: Long, miner: Long): Unit = {
    tx.inputs.map(i => bytesToId(i.boxId)) shouldBe IndexedSeq(bytesToId(input.id))
    tx.inputs.head.spendingProof.proof.isEmpty shouldBe true
    tx.inputs.head.spendingProof.extension.values.isEmpty shouldBe true
    tx.dataInputs shouldBe empty
    tx.outputs.size shouldBe 4
    val Seq(succ, t, l, m) = tx.outputCandidates
    succ.value shouldBe input.value - (treasury + lp + miner)
    succ.propositionBytes.toSeq shouldBe input.propositionBytes.toSeq
    succ.additionalTokens.toArray.toSeq.map(x => Base16.encode(x._1.toArray) -> x._2) shouldBe
      Seq(e.emissionNftId -> 1L)
    succ.additionalRegisters shouldBe Map(R4 -> input.additionalRegisters(R4), R5 -> input.additionalRegisters(R5))
    Seq(t, l, m).map(_.value) shouldBe Seq(treasury, lp, miner)
    t.ergoTree shouldBe e.treasuryAccumulationTree
    l.ergoTree shouldBe e.lpAccumulationTree
    m.ergoTree shouldBe ErgoTreePredef.rewardOutputScript(2880, pk)
    Seq(t, l, m).foreach { o =>
      o.additionalTokens.isEmpty shouldBe true
      o.additionalRegisters shouldBe empty
    }
    tx.outputCandidates.foreach(_.creationHeight shouldBe height)
  }

  private def checkTerminalLayout(tx: ErgoTransaction, input: ErgoBox, height: Int, pk: ProveDlog,
                                  treasury: Long, lp: Long, miner: Long): Unit = {
    tx.inputs.map(i => bytesToId(i.boxId)) shouldBe IndexedSeq(bytesToId(input.id))
    tx.outputs.size shouldBe 3
    tx.outputCandidates.map(_.value) shouldBe Seq(treasury, lp, miner)
    treasury + lp + miner shouldBe input.value
    tx.outputCandidates.map(_.ergoTree) shouldBe
      Seq(e.treasuryAccumulationTree, e.lpAccumulationTree, ErgoTreePredef.rewardOutputScript(2880, pk))
    tx.outputCandidates.foreach { o =>
      o.additionalTokens.isEmpty shouldBe true // the NFT is burned
      o.additionalRegisters shouldBe empty
      o.creationHeight shouldBe height
    }
  }

  property("coinbase miner script: rewardOutputScript(2880, pk) has the bytes emission.es concatenates") {
    YoloEmissionTx.CoinbaseMinerRewardDelay shouldBe 2880
    Seq(defaultMinerPk, otherPk).foreach { pk =>
      Base16.encode(YoloEmissionTx.minerOutputScript(pk).bytes) shouldBe
        "100204802d08cd" + Base16.encode(groupElemToBytes(pk.value)) + "ea02d192a39a8cc7a70173007301"
    }
    // the fee path keeps Ergo's 720 delay
    devnet.chainSettings.monetary.minerRewardDelay shouldBe 720
  }

  property("height 1: the candidate-path emission tx passes the real emission.es, then a block moves the tracked box") {
    val us0 = genesisState()
    val genesisBox = us0.emissionBoxOpt.get
    genesisBox.value shouldBe GenesisValue
    us0.stateContext.currentHeight shouldBe 0

    val tx = CandidateGenerator.collectEmission(us0, defaultMinerPk, us0.stateContext).get
    checkNormalLayout(tx, genesisBox, 1, defaultMinerPk, 1500000000L, 1125000000L, 34875000000L)
    tx.outputs.head.value shouldBe GenesisValue - 37500000000L

    val ctx = upcoming(us0.stateContext, defaultMinerPk)
    ctx.currentHeight shouldBe 1
    val txCost = validate(us0, tx, ctx).get

    // script cost of the emission input alone, as rule 119 computes it
    val verifier = ErgoInterpreter(ctx.currentParameters)
    val input = tx.inputs.head
    val ectx = new ErgoContext(ctx, TransactionContext(IndexedSeq(genesisBox), IndexedSeq.empty, tx),
      InputContext(0, input.spendingProof.extension), ctx.currentParameters.maxBlockCost.toLong, 0L)
    val (scriptOk, scriptCost) = verifier.verify(genesisBox.ergoTree, ectx, input.spendingProof, tx.messageToSign).get
    scriptOk shouldBe true
    info(s"normal-path emission tx at height 1: tx validation cost $txCost, emission.es script cost $scriptCost, " +
      s"tx size ${tx.size} bytes")

    // the candidate assembly loop keeps it
    val (collected, invalid) = CandidateGenerator.collectTxs(defaultMinerPk, ctx.currentParameters.maxBlockCost,
      ctx.currentParameters.maxBlockSize, us0, ctx, Seq(tx))
    collected.map(_.id) shouldBe Seq(tx.id)
    invalid shouldBe empty

    // block application (state context from the header: pre-header key and height)
    val (us1, fb) = mine(us0, None, Seq(tx))
    fb.header.height shouldBe 1
    us1.emissionBoxOpt shouldBe Some(tx.outputs.head)
    us1.boxById(genesisBox.id) shouldBe None
    tx.outputs.foreach(o => us1.boxById(o.id) shouldBe Some(o))
    us1.closeStorage()
  }

  property("heights 1 to 4: each block's emission tx spends the previous successor") {
    var us = genesisState()
    var parent: Option[ErgoFullBlock] = None
    (1 to 4).foreach { h =>
      val before = us.emissionBoxOpt.get
      before.value shouldBe GenesisValue - 37500000000L * (h - 1)
      before.creationHeight shouldBe h - 1
      val tx = CandidateGenerator.collectEmission(us, defaultMinerPk, us.stateContext).get
      checkNormalLayout(tx, before, h, defaultMinerPk, 1500000000L, 1125000000L, 34875000000L)
      validate(us, tx, upcoming(us.stateContext, defaultMinerPk)) shouldBe 'success
      val (next, fb) = mine(us, parent, Seq(tx))
      fb.header.height shouldBe h
      next.emissionBoxOpt shouldBe Some(tx.outputs.head)
      us = next
      parent = Some(fb)
    }
    us.emissionBoxOpt.get.value shouldBe GenesisValue - 4 * 37500000000L
    us.closeStorage()
  }

  property("halving boundaries 1,051,199 / 1,051,200 and 2,102,399 / 2,102,400, and the tail start 3,153,599 / " +
    "3,153,600: valid, right amounts") {
    // the emission box "at" height h is built directly (value boxValueAt(h), creation height h - 1) and put into a
    // genesis-like state; the validation context has a generated last header at h - 1
    val rules = devnet.chainSettings.yoloEmissionRulesOpt.get
    val cases = Seq(
      (1051199, 1500000000L, 1125000000L, 34875000000L),
      (1051200, 750000000L, 562500000L, 17437500000L),
      (2102399, 750000000L, 562500000L, 17437500000L),
      (2102400, 375000000L, 281250000L, 8718750000L),
      (3153599, 375000000L, 281250000L, 8718750000L),
      (3153600, 187500000L, 140625000L, 4359375000L))
    cases.foreach { case (h, treasury, lp, miner) =>
      withClue(s"height $h: ") {
        val genesis = ErgoState.genesisBoxes(devnet.chainSettings)
        val value = rules.boxValueAt(h)
        value should be > (treasury + lp + miner)
        val input = new ErgoBoxCandidate(value, e.emissionTree, h - 1, Colls.fromItems(nft),
          genesis.head.additionalRegisters).toBox(bytesToId(Array.fill(32)(7: Byte)), 0)
        val us = stateOf(devnet, input +: genesis.tail)
        val sc = atHeight(us.stateContext, h - 1)
        // the switch-on path of collectRewards, as collectEmission calls it
        val tx = CandidateGenerator.collectRewards(Some(input), h - 1, Seq.empty, defaultMinerPk, sc).head
        tx.id shouldBe YoloEmissionTx.build(input, h, defaultMinerPk, e).id
        checkNormalLayout(tx, input, h, defaultMinerPk, treasury, lp, miner)
        val ctx = upcoming(sc, defaultMinerPk)
        ctx.currentHeight shouldBe h
        validate(us, tx, ctx) shouldBe 'success
        // the neighbouring arm's reward (ERG still preserved) is rejected at the boundary
        val wrongReward = Seq(h - 1, h + 1).map(x => YoloEmissionRules.rewardAtHeight(x))
          .find(_ != YoloEmissionRules.rewardAtHeight(h)).get
        val w = YoloEmissionRules.split(wrongReward)
        val IndexedSeq(succ, t, l, m) = tx.outputCandidates
        rejectedByScript(us, withOutputs(tx)(IndexedSeq(box(succ, value = value - wrongReward),
          box(t, value = w.treasury), box(l, value = w.lp), box(m, value = w.miner))), ctx)
        us.closeStorage()
      }
    }
  }

  property("terminal path: small genesis value, 3 outputs, NFT burned, then no emission box and no emission tx") {
    // 122.5 YOLO: heights 1 to 3 pay 37.5 each (normal), height 4 spends the last 10 YOLO (terminal)
    val s = withGenesisValue(1225L * Coin / 10)
    s.chainSettings.yoloEmissionRulesOpt.get.terminalHeight shouldBe 4
    var us = genesisState(s)
    var parent: Option[ErgoFullBlock] = None
    (1 to 3).foreach { _ =>
      val tx = CandidateGenerator.collectEmission(us, defaultMinerPk, us.stateContext).get
      tx.outputs.size shouldBe 4
      val (next, fb) = mine(us, parent, Seq(tx))
      us = next
      parent = Some(fb)
    }
    val last = us.emissionBoxOpt.get
    last.value shouldBe 10L * Coin
    val tx = CandidateGenerator.collectEmission(us, defaultMinerPk, us.stateContext).get
    checkTerminalLayout(tx, last, 4, defaultMinerPk, 400000000L, 300000000L, 9300000000L)
    validate(us, tx, upcoming(us.stateContext, defaultMinerPk)) shouldBe 'success
    val (after, _) = mine(us, parent, Seq(tx))
    after.emissionBoxOpt shouldBe None
    after.stateContext.currentHeight shouldBe 4
    CandidateGenerator.collectEmission(after, defaultMinerPk, after.stateContext) shouldBe None
    CandidateGenerator.collectRewards(None, 4, Seq.empty, defaultMinerPk, after.stateContext) shouldBe empty
    after.closeStorage()
  }

  property("terminal path at equality (box value == reward): the split of the box equals the normal split") {
    // 75 YOLO: height 1 leaves exactly 37.5 YOLO, spent at height 2 on the terminal path
    val s = withGenesisValue(75L * Coin)
    s.chainSettings.yoloEmissionRulesOpt.get.terminalHeight shouldBe 2
    val us0 = genesisState(s)
    val (us1, fb1) = mine(us0, None, CandidateGenerator.collectEmission(us0, defaultMinerPk, us0.stateContext).toSeq)
    val last = us1.emissionBoxOpt.get
    last.value shouldBe YoloEmissionRules.rewardAtHeight(2)
    YoloEmissionTx.isTerminal(last.value, 2) shouldBe true
    val tx = CandidateGenerator.collectEmission(us1, defaultMinerPk, us1.stateContext).get
    checkTerminalLayout(tx, last, 2, defaultMinerPk, 1500000000L, 1125000000L, 34875000000L)
    validate(us1, tx, upcoming(us1.stateContext, defaultMinerPk)) shouldBe 'success
    val (us2, _) = mine(us1, Some(fb1), Seq(tx))
    us2.emissionBoxOpt shouldBe None
    CandidateGenerator.collectEmission(us2, defaultMinerPk, us2.stateContext) shouldBe None
    us2.closeStorage()
  }

  property("negative controls at height 1: each mutation is rejected by the emission script (rule 119)") {
    val us = genesisState()
    val input = us.emissionBoxOpt.get
    val ctx = upcoming(us.stateContext, defaultMinerPk)
    val honest = YoloEmissionTx.build(input, 1, defaultMinerPk, e)
    validate(us, honest, ctx) shouldBe 'success
    val IndexedSeq(succ, t, l, m) = honest.outputCandidates

    val mutations: Seq[(String, ErgoTransaction)] = Seq(
      "miner output at delay 720" ->
        withOutputs(honest)(IndexedSeq(succ, t, l, box(m, tree = ErgoTreePredef.rewardOutputScript(720, defaultMinerPk)))),
      "miner output to a key other than the pre-header's" ->
        YoloEmissionTx.build(input, 1, otherPk, e),
      "treasury +1 nano (miner -1)" ->
        withOutputs(honest)(IndexedSeq(succ, box(t, value = t.value + 1), l, box(m, value = m.value - 1))),
      "treasury -1 nano (miner +1)" ->
        withOutputs(honest)(IndexedSeq(succ, box(t, value = t.value - 1), l, box(m, value = m.value + 1))),
      "treasury and LP shares swapped" ->
        withOutputs(honest)(IndexedSeq(succ, l, t, m)),
      "a 5th output (1 YOLO of the miner share split off to the same miner script)" ->
        withOutputs(honest)(IndexedSeq(succ, t, l, box(m, value = m.value - Coin), box(m, value = Coin))),
      "successor with an extra register R6" ->
        withOutputs(honest)(IndexedSeq(box(succ, regs = succ.additionalRegisters ++
          Map(R6 -> ByteArrayConstant(Array[Byte](1)))), t, l, m)),
      "successor with an extra (minted) token" ->
        withOutputs(honest)(IndexedSeq(box(succ, tokens = Seq(nft,
          (Digest32Coll @@ Colls.fromArray(input.id)) -> 1000L)), t, l, m)),
      "successor creation height nextHeight - 1" ->
        withOutputs(honest)(IndexedSeq(box(succ, height = 0), t, l, m)),
      "Ergo-style 2-output tx (the upstream builder with the switch off)" -> {
        val off = ErgoStateContext.empty(devnet.chainSettings.copy(yolo = YoloSettings.disabled), YoloLaunchParameters)
        val ergoTx = CandidateGenerator.collectRewards(Some(input), 0, Seq.empty, defaultMinerPk,
          upcoming(off, defaultMinerPk)).head
        ergoTx.outputs.size shouldBe 2
        ergoTx.outputs(1).ergoTree shouldBe ErgoTreePredef.rewardOutputScript(720, defaultMinerPk)
        ergoTx
      })

    mutations.foreach { case (name, tx) =>
      withClue(s"$name: ") {
        tx.id should not be honest.id
        rejectedByScript(us, tx, ctx)
      }
    }
    us.closeStorage()
  }

  // an emission box "at" height h (spent in the block at h): value `value`, creation height h - 1
  private def emissionInputAt(h: Int, value: Long): ErgoBox = {
    val genesis = ErgoState.genesisBoxes(devnet.chainSettings)
    new ErgoBoxCandidate(value, e.emissionTree, h - 1, Colls.fromItems(nft), genesis.head.additionalRegisters)
      .toBox(bytesToId(Array.fill(32)(7: Byte)), 0)
  }

  // a UTXO state holding `input` in place of the genesis emission box, with a last header at h - 1, and the
  // emission tx the candidate path builds for height h
  private def atTail(h: Int, input: ErgoBox): (UtxoState, ErgoStateContext, ErgoTransaction) = {
    val us = stateOf(devnet, input +: ErgoState.genesisBoxes(devnet.chainSettings).tail)
    val sc = atHeight(us.stateContext, h - 1)
    val tx = CandidateGenerator.collectRewards(Some(input), h - 1, Seq.empty, defaultMinerPk, sc).head
    tx.id shouldBe YoloEmissionTx.build(input, h, defaultMinerPk, e).id
    (us, upcoming(sc, defaultMinerPk), tx)
  }

  private lazy val p2pk: ErgoTree = P2PKAddress(otherPk)(devnet.addressEncoder).script

  property("devnet genesis: last normal spend at 26,258,940 from a 6.25 YOLO box, terminal spend at 26,258,941, " +
    "no emission at 26,258,942") {
    val rules = devnet.chainSettings.yoloEmissionRulesOpt.get
    rules.genesisValue shouldBe GenesisValue
    rules.terminalHeight shouldBe 26258941L
    rules.boxValueAt(26258940L) shouldBe 6250000000L

    // the last normal spend: 4.6875 YOLO paid, the successor holds the last 1.5625 YOLO
    val (us1, ctx1, tx1) = atTail(26258940, emissionInputAt(26258940, rules.boxValueAt(26258940L)))
    checkNormalLayout(tx1, us1.emissionBoxOpt.get, 26258940, defaultMinerPk, 187500000L, 140625000L, 4359375000L)
    tx1.outputs.head.value shouldBe 1562500000L
    validate(us1, tx1, ctx1) shouldBe 'success
    us1.closeStorage()

    // the terminal spend of that successor: 1.5625 YOLO split 4 / 3 / 93, NFT burned
    val last = tx1.outputs.head
    rules.boxValueAt(26258941L) shouldBe last.value
    val (us2, ctx2, tx2) = atTail(26258941, last)
    checkTerminalLayout(tx2, last, 26258941, defaultMinerPk, 62500000L, 46875000L, 1453125000L)
    validate(us2, tx2, ctx2) shouldBe 'success
    // height 26,258,942: the box is gone, so there is no emission transaction
    rules.boxValueAt(26258942L) shouldBe 0L
    rules.isTerminalHeight(26258942L) shouldBe false
    CandidateGenerator.collectRewards(None, 26258941, Seq.empty, defaultMinerPk,
      atHeight(us2.stateContext, 26258941)) shouldBe empty
    us2.closeStorage()
  }

  property("terminal path: an odd box value (1,562,500,007 nano) is split with the rounding to the miner") {
    val input = emissionInputAt(26258941, 1562500007L)
    val (us, ctx, tx) = atTail(26258941, input)
    checkTerminalLayout(tx, input, 26258941, defaultMinerPk, 62500000L, 46875000L, 1453125007L)
    validate(us, tx, ctx) shouldBe 'success
    us.closeStorage()
  }

  property("terminal-path negative controls: each mutation is rejected by the emission script (rule 119)") {
    val h = 26258941
    val input = emissionInputAt(h, 1562500000L)
    val (us, ctx, honest) = atTail(h, input)
    validate(us, honest, ctx) shouldBe 'success
    val IndexedSeq(t, l, m) = honest.outputCandidates
    val mutations: Seq[(String, ErgoTransaction)] = Seq(
      "treasury share to a P2PK" -> withOutputs(honest)(IndexedSeq(box(t, tree = p2pk), l, m)),
      "LP share with a register R4" ->
        withOutputs(honest)(IndexedSeq(t, box(l, regs = Map(R4 -> ByteArrayConstant(Array[Byte](1)))), m)),
      "treasury share with creation height h - 1" -> withOutputs(honest)(IndexedSeq(box(t, height = h - 1), l, m)),
      "miner output with creation height h - 1" -> withOutputs(honest)(IndexedSeq(t, l, box(m, height = h - 1))),
      "a 4th output (0.1 YOLO of the miner share split off to the same miner script)" ->
        withOutputs(honest)(IndexedSeq(t, l, box(m, value = m.value - Coin / 10), box(m, value = Coin / 10))))
    mutations.foreach { case (name, tx) =>
      withClue(s"$name: ") {
        tx.id should not be honest.id
        rejectedByScript(us, tx, ctx)
      }
    }
    us.closeStorage()
  }

  property("normal-path isolated controls at height 1: each mutation is rejected by the emission script (rule 119)") {
    val us = genesisState()
    val input = us.emissionBoxOpt.get
    val ctx = upcoming(us.stateContext, defaultMinerPk)
    val honest = YoloEmissionTx.build(input, 1, defaultMinerPk, e)
    validate(us, honest, ctx) shouldBe 'success
    val IndexedSeq(succ, t, l, m) = honest.outputCandidates
    val mutations: Seq[(String, ErgoTransaction)] = Seq(
      "treasury value at the LP accumulation tree" ->
        withOutputs(honest)(IndexedSeq(succ, box(t, tree = e.lpAccumulationTree), l, m)),
      "treasury value at a P2PK" -> withOutputs(honest)(IndexedSeq(succ, box(t, tree = p2pk), l, m)),
      "LP share with a register R4" ->
        withOutputs(honest)(IndexedSeq(succ, t, box(l, regs = Map(R4 -> ByteArrayConstant(Array[Byte](1)))), m)),
      "miner output with creation height h - 1" -> withOutputs(honest)(IndexedSeq(succ, t, l, box(m, height = 0))))
    mutations.foreach { case (name, tx) =>
      withClue(s"$name: ") {
        tx.id should not be honest.id
        rejectedByScript(us, tx, ctx)
      }
    }
    us.closeStorage()
  }

  property("a block with the coinbase, a fee-paying tx and the fee tx applies") {
    // genesis plus a spendable box, for the fee-paying transaction
    val spendable = new ErgoBoxCandidate(10L * Coin, TrueTree, 0).toBox(bytesToId(Array.fill(32)(9: Byte)), 0)
    val us0 = stateOf(devnet, ErgoState.genesisBoxes(devnet.chainSettings) :+ spendable)
    val ctx = upcoming(us0.stateContext, defaultMinerPk)
    val coinbase = CandidateGenerator.collectEmission(us0, defaultMinerPk, us0.stateContext).get
    val fee = Coin / 100
    val feeProp = ErgoTreePredef.feeProposition(devnet.chainSettings.monetary.minerRewardDelay)
    val paying = ErgoTransaction(IndexedSeq(new Input(spendable.id, ProverResult.empty)), IndexedSeq.empty,
      IndexedSeq(new ErgoBoxCandidate(spendable.value - fee, TrueTree, 1), new ErgoBoxCandidate(fee, feeProp, 1)))
    val (collected, invalid) = CandidateGenerator.collectTxs(defaultMinerPk, ctx.currentParameters.maxBlockCost,
      ctx.currentParameters.maxBlockSize, us0, ctx, Seq(coinbase, paying))
    invalid shouldBe empty
    collected.size shouldBe 3
    collected.take(2).map(_.id) shouldBe Seq(coinbase.id, paying.id)
    val feeTx = collected(2)
    feeTx.inputs.map(i => bytesToId(i.boxId)) shouldBe IndexedSeq(bytesToId(paying.outputs(1).id))
    // the fee output stays at Ergo's 720 delay, the coinbase miner output at 2880
    feeTx.outputs.map(_.ergoTree) shouldBe IndexedSeq(ErgoTreePredef.rewardOutputScript(720, defaultMinerPk))
    feeTx.outputs.map(_.value).sum shouldBe fee
    coinbase.outputs(3).ergoTree shouldBe ErgoTreePredef.rewardOutputScript(2880, defaultMinerPk)

    val (us1, fb) = mine(us0, None, collected)
    fb.header.height shouldBe 1
    fb.transactions.map(_.id) shouldBe collected.map(_.id)
    us1.emissionBoxOpt shouldBe Some(coinbase.outputs.head)
    (coinbase.outputs ++ paying.outputs.take(1) ++ feeTx.outputs).foreach(o => us1.boxById(o.id) shouldBe Some(o))
    us1.boxById(spendable.id) shouldBe None
    us1.boxById(paying.outputs(1).id) shouldBe None
    us1.closeStorage()
  }

  property("custom miner key (the /mining/candidateWithTxsAndPk path): the tx is bound to that key") {
    // generateCandidate passes the request's key to collectEmission and to the upcoming context
    val us0 = genesisState()
    val tx = CandidateGenerator.collectEmission(us0, otherPk, us0.stateContext).get
    checkNormalLayout(tx, us0.emissionBoxOpt.get, 1, otherPk, 1500000000L, 1125000000L, 34875000000L)
    val own = upcoming(us0.stateContext, otherPk)
    validate(us0, tx, own) shouldBe 'success
    rejectedByScript(us0, tx, upcoming(us0.stateContext, defaultMinerPk))
    def collect(ctx: ErgoStateContext) = CandidateGenerator.collectTxs(otherPk, ctx.currentParameters.maxBlockCost,
      ctx.currentParameters.maxBlockSize, us0, ctx, Seq(tx))
    collect(own)._1.map(_.id) shouldBe Seq(tx.id)
    collect(upcoming(us0.stateContext, defaultMinerPk))._2 shouldBe Seq(tx.id)
    // a block whose header carries that key applies
    val (us1, fb) = mine(us0, None, Seq(tx), otherSecret)
    groupElemToBytes(fb.header.powSolution.pk).toSeq shouldBe groupElemToBytes(otherPk.value).toSeq
    us1.emissionBoxOpt shouldBe Some(tx.outputs.head)
    us1.closeStorage()
  }

  property("emission-only fallback (YOLO): the emission tx is validated, a wrong one refused; switch off unchanged") {
    val us = genesisState()
    val input = us.emissionBoxOpt.get
    val ctx = upcoming(us.stateContext, defaultMinerPk)
    val honest = YoloEmissionTx.build(input, 1, defaultMinerPk, e)
    CandidateGenerator.yoloFallbackEmissionCheck(devnet, us, honest, ctx) shouldBe 'success
    // a deliberately wrong emission tx: bound to a key other than the candidate's
    val wrong = YoloEmissionTx.build(input, 1, otherPk, e)
    val res = CandidateGenerator.yoloFallbackEmissionCheck(devnet, us, wrong, ctx)
    res.isFailure shouldBe true
    res.failed.get.getMessage should include(ScriptRejected)
    // switch off: no check, as upstream considers the emission tx valid
    val off = devnet.copy(chainSettings = devnet.chainSettings.copy(yolo = YoloSettings.disabled))
    CandidateGenerator.yoloFallbackEmissionCheck(off, us, wrong, ctx) shouldBe 'success
    us.closeStorage()
  }

}
