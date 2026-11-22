package org.ergoplatform.mining

import org.ergoplatform.consensus.ProgressInfo
import org.ergoplatform.mining.difficulty.DifficultySerializer
import org.ergoplatform.modifiers.history.BlockTransactions
import org.ergoplatform.modifiers.history.extension.Extension
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.history.popow.NipopowAlgos
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.modifiers.{BlockSection, ErgoFullBlock, NonHeaderBlockSection}
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.nodeView.history.extra.{ExtraIndex, ExtraIndexer, IndexedErgoBox, NumericBoxIndex, StorageRentBox}
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.mempool.{ErgoMemPool, YoloFeeOutputPolicy}
import org.ergoplatform.nodeView.mempool.ErgoMemPoolUtils.ProcessingOutcome
import org.ergoplatform.local.CleanupWorker
import org.ergoplatform.nodeView.state.{ErgoState, ErgoStateContext, UpcomingStateContext, UtxoState, VotingData}
import org.ergoplatform.settings.Constants.{FalseTree, TrueTree}
import org.ergoplatform.settings.{Args, ErgoSettings, ErgoSettingsReader, ErgoValidationSettingsUpdate, NetworkType,
  YoloEmissionSettings, YoloLaunchParameters}
import org.ergoplatform.utils.{ErgoCorePropertyTest, StorageRentTestHelpers}
import org.ergoplatform.wallet.interpreter.ErgoInterpreter
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, ErgoTreePredef, Input}
import scorex.util.{ModifierId, bytesToId}
import sigma.Colls
import sigma.Extensions.CollBytesOps
import sigma.ast.LongConstant
import sigma.data.Digest32Coll
import sigma.interpreter.ProverResult

import java.io.File
import java.nio.ByteBuffer
import scala.collection.mutable
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

/**
  * Block assembly with the YOLO launch parameters (block cost limit 1,500,000, assembly safe gap 150,000) for a
  * rent-collecting miner past the first storage period: the emission transaction, the miner's own storage rent claim
  * of `StorageRentClaimBuilder.MaxClaims` = 100 boxes and the fee transaction take their share of the block first.
  *
  * A pool transaction at the relay limit (`ergo.node.maxTransactionCost` in yolo-testnet.conf) is included beside
  * them, and the measured cost of each piece is reported.
  */
class YoloBlockAssemblySpec extends ErgoCorePropertyTest with StorageRentTestHelpers {
  import org.ergoplatform.utils.ErgoCoreTestConstants.{defaultMinerPk, defaultMinerSecretNumber, defaultTimestamp,
    genesisStateDigest, powScheme, validationSettingsNoIl}
  import org.ergoplatform.utils.generators.ErgoCoreGenerators.defaultHeaderGen

  private lazy val devnetPath: String =
    new File(getClass.getClassLoader.getResource("yolo-devnet.conf").toURI).getAbsolutePath
  // the full, checked read: exits the JVM if the devnet conf fails a start-up check
  private lazy val devnet: ErgoSettings = ErgoSettingsReader
    .read(Args(Some(devnetPath), Some(NetworkType.TestNet)))
    .copy(directory = createTempDir.getAbsolutePath)
  private lazy val e: YoloEmissionSettings = devnet.chainSettings.yolo.emission
  private lazy val switchOff: ErgoSettings = devnet.copy(chainSettings = devnet.chainSettings.copy(
    yolo = devnet.chainSettings.yolo.copy(enabled = false)))

  /** Height of the best block; the candidate is for `next`, one storage period and 100 blocks after genesis */
  private val tip: Int = StoragePeriod + 99
  private val next: Int = tip + 1

  private val Coin: Long = 1000000000L
  private val Fee: Long = Coin / 100
  private val PadValue: Long = Coin / 1000
  private lazy val feeProp = ErgoTreePredef.feeProposition(devnet.chainSettings.monetary.minerRewardDelay)

  /** Relay limit as yolo-testnet.conf ships it, and the candidate's cost budget, as createCandidate computes it */
  private lazy val relayLimit: Int = devnet.nodeSettings.maxTransactionCost
  private val BlockCost: Int = YoloLaunchParameters.maxBlockCost
  private val Budget: Int = BlockCost - CandidateGenerator.safeGap(BlockCost)

  /** The YOLO emission box for a block at `next` */
  private lazy val emissionBox: ErgoBox = {
    val genesis = ErgoState.genesisBoxes(devnet.chainSettings)
    val nft = (Digest32Coll @@ Colls.fromArray(e.emissionNftIdBytes)) -> 1L
    new ErgoBoxCandidate(devnet.chainSettings.yoloEmissionRulesOpt.get.boxValueAt(next), e.emissionTree, tip,
      Colls.fromItems(nft), genesis.head.additionalRegisters).toBox(bytesToId(Array.fill(32)(7: Byte)), 0)
  }

  /** 100 boxes past the storage period, `false` script: claimable only through the rent branch, and recreated */
  private lazy val rentBoxes: Seq[ErgoBox] =
    (0 until StorageRentClaimBuilder.MaxClaims).map(i => boxAt(FalseTree, next - StoragePeriod - 10, seed = 1000 + i))

  /** As `rentBoxes`, each carrying `TokensPerBox` tokens of its own (for the cost of tokens in a claim) */
  private val TokensPerBox = 10
  private lazy val tokenRentBoxes: Seq[ErgoBox] = (0 until StorageRentClaimBuilder.MaxClaims).map { i =>
    val tokens = (0 until TokensPerBox).map { j =>
      (Digest32Coll @@ Colls.fromArray(ByteBuffer.allocate(32).putInt(0x70000000).putInt(i).putInt(j).array)) -> 1L
    }
    new ErgoBox(BoxValue, FalseTree, Colls.fromItems(tokens: _*), Map.empty,
      bytesToId(ByteBuffer.allocate(32).putInt(0x66666666).putInt(i).array), 0, next - StoragePeriod - 10)
  }

  private def tokenId(prefix: Int, i: Int, j: Int): ErgoBox.TokenId =
    Digest32Coll @@ Colls.fromArray(ByteBuffer.allocate(32).putInt(prefix).putInt(i).putInt(j).array)

  private def boxId(prefix: Int, i: Int): ModifierId = bytesToId(ByteBuffer.allocate(32).putInt(prefix).putInt(i).array)

  /** A `false`-script box past the storage period carrying `tokens` (amount 1 each), no registers */
  private def oldBox(prefix: Int, i: Int, tokens: Seq[ErgoBox.TokenId], value: Long = BoxValue): ErgoBox =
    new ErgoBox(value, FalseTree, Colls.fromItems(tokens.map(_ -> 1L): _*), Map.empty, boxId(prefix, i), 0,
      next - StoragePeriod - 10)

  /**
    * Stall window: 100 recreated boxes with 28 distinct tokens each (2,800 ids). Upstream builds one
    * claim of all 100 costing 1,345,100, inside [budget - emission tx, budget] = [1,336,891, 1,350,000]
    */
  private lazy val windowBoxes: Seq[ErgoBox] =
    (0 until 100).map(i => oldBox(0x71000000, i, (0 until 28).map(j => tokenId(0x71000000, i, j))))

  /**
    * Burned variant: 100 boxes of 0.002 coin, under their storage fee, each holding the same 111 ids.
    * Upstream's claim costs 1,346,100, also inside the window
    */
  private lazy val burnedIds: Seq[ErgoBox.TokenId] = (0 until 111).map(j => tokenId(0x72000000, 0, j))
  private lazy val burnedBoxes: Seq[ErgoBox] = (0 until 100).map(i => oldBox(0x72000000, i, burnedIds, Coin / 500))

  /**
    * Mixed case: 50 honest token-free boxes interleaved with 50 boxes of 57 distinct tokens each.
    * Upstream's claim costs 1,365,100, above the 1,350,000 budget
    */
  private lazy val mixedBoxes: Seq[ErgoBox] = (0 until 100).map { i =>
    if (i % 2 == 0) oldBox(0x73000000, i, Seq.empty)
    else oldBox(0x73000000, i, (0 until 57).map(j => tokenId(0x73000000, i, j)))
  }

  /** 100 recreated boxes all holding the same 28 ids (shared ids on a recreation) */
  private lazy val sharedIdBoxes: Seq[ErgoBox] =
    (0 until 100).map(i => oldBox(0x74000000, i, (0 until 28).map(j => tokenId(0x74000000, 0, j))))

  /** Spendable `true` boxes for the pool transactions (one transaction id per box, distinct ids) */
  private def spendable(from: Int, n: Int): IndexedSeq[ErgoBox] = (from until from + n).map { i =>
    new ErgoBoxCandidate(Coin, TrueTree, tip - 10)
      .toBox(bytesToId(ByteBuffer.allocate(32).putInt(0x55555555).putInt(i).array), 0)
  }

  private lazy val forAtLimit: IndexedSeq[ErgoBox] = spendable(0, 600)
  private lazy val forOversized: IndexedSeq[ErgoBox] = spendable(1000, 700)
  private lazy val forSmall: IndexedSeq[ErgoBox] = spendable(2000, 300)

  /** Amount of the fee tokens: 2^62, a 9-byte VLQ */
  private val BigAmount: Long = 1L << 62

  /** Spendable `true` boxes holding 60 distinct tokens of amount 2^62 each (for fee outputs carrying tokens) */
  private lazy val forTokenFee: IndexedSeq[ErgoBox] = (0 until 3).map { i =>
    val tokens = (0 until 60).map(j => tokenId(0x75000000, i, j) -> BigAmount)
    new ErgoBox(Coin, TrueTree, Colls.fromItems(tokens: _*), Map.empty, boxId(0x75000000, i), 0, tip - 10)
  }

  /** The miner reward script the fee tx pays to (`collectRewards`) */
  private lazy val minerRewardProp = ErgoTreePredef.rewardOutputScript(devnet.chainSettings.monetary.minerRewardDelay,
    defaultMinerPk)

  private val AnyTxId: ModifierId = bytesToId(Array.fill(32)(0: Byte))

  /** Serialized length of a box made of `c`, as an output at index 0 */
  private def sizeOf(c: ErgoBoxCandidate): Int = c.toBox(AnyTxId, 0).bytes.length

  /** Smallest amount whose VLQ takes `len` bytes (7 bits per byte) */
  private def amountOfLength(len: Int): Long = if (len <= 1) 1L else 1L << (7 * (len - 1))

  /**
    * Sender's token fee boxes: 100 token ids with amounts set one VLQ byte at a time until the miner output holding all
    * of them, at a value of 2^21 - 1 (the largest 3-byte VLQ), is exactly 4,096 bytes
    */
  private val ProbeBValue: Long = (1L << 21) - 1
  private lazy val probeBTokens: IndexedSeq[(ErgoBox.TokenId, Long)] = {
    val ids = (0 until 100).map(j => tokenId(0x78000000, 0, j))
    val lengths = mutable.ArrayBuffer.fill(100)(1)
    def tokens = ids.zip(lengths.map(amountOfLength))
    def size = sizeOf(new ErgoBoxCandidate(ProbeBValue, minerRewardProp, next, Colls.fromItems(tokens: _*)))
    var j = 0
    while (size < ErgoBox.MaxBoxSize) {
      if (lengths(j) == 9) j += 1 else lengths(j) += 1
    }
    tokens
  }
  /** The `true` box that sender spends: the 100 tokens and 1 coin */
  private lazy val probeBInput: ErgoBox =
    new ErgoBox(Coin, TrueTree, Colls.fromItems(probeBTokens: _*), Map.empty, boxId(0x78000000, 0), 0, tip - 10)

  /** A `true` box of 1 coin holding 101 distinct tokens */
  private lazy val probeCInput: ErgoBox = new ErgoBox(Coin, TrueTree,
    Colls.fromItems((0 until 101).map(j => tokenId(0x79000000, 0, j) -> 1L): _*), Map.empty, boxId(0x79000000, 0), 0,
    tip - 10)

  /**
    * The `false`-script box past the storage period with the most distinct tokens (amount 1) that fits in 4,096
    * bytes, recreated or not
    */
  private lazy val largestTokenBox: ErgoBox = {
    def withTokens(n: Int): ErgoBox = oldBox(0x7a000000, n, (0 until n).map(j => tokenId(0x7a000000, 0, j)))
    val n = Iterator.from(1).takeWhile(k => withTokens(k).bytes.length <= ErgoBox.MaxBoxSize).toSeq.last
    withTokens(n)
  }

  /** A box past the storage period whose one token has amount 0: the builder admits it, validation does not */
  private lazy val zeroAmountBox: ErgoBox = new ErgoBox(BoxValue, FalseTree,
    Colls.fromItems(tokenId(0x7b000000, 0, 0) -> 0L), Map.empty, boxId(0x7b000000, 0), 0, next - StoragePeriod - 10)

  /** Two boxes past the storage period worth 2^62 each: each is claimable alone, together their values overflow */
  private lazy val hugeBoxes: Seq[ErgoBox] = (0 until 2).map(i => oldBox(0x7c000000, i, Seq.empty, 1L << 62))

  /** Spends `input` (a `true` box with tokens) into a token-free change output and a fee output carrying its tokens */
  private def tokenFeeTx(input: ErgoBox): ErgoTransaction =
    ErgoTransaction(IndexedSeq(new Input(input.id, ProverResult.empty)), IndexedSeq.empty, IndexedSeq(
      new ErgoBoxCandidate(input.value - Fee, TrueTree, next),
      new ErgoBoxCandidate(Fee, feeProp, next, input.additionalTokens)))

  private def ctxAtTip(s: ErgoSettings): ErgoStateContext = {
    val header = defaultHeaderGen.sample.get.copy(version = YoloLaunchParameters.blockVersion, height = tip)
    new ErgoStateContext(Seq(header), None, genesisStateDigest, YoloLaunchParameters, validationSettingsNoIl,
      VotingData.empty)(s.chainSettings)
  }

  /** UTXO state at `tip`, with the YOLO launch parameters, holding the emission box, the rent boxes and spendables */
  private def stateAtTip(s: ErgoSettings = devnet): UtxoState = {
    val us = utxoStateAt(Seq(emissionBox) ++ rentBoxes ++ tokenRentBoxes ++ windowBoxes ++ burnedBoxes ++ mixedBoxes ++
      sharedIdBoxes ++ forAtLimit ++ forOversized ++ forSmall ++ forTokenFee ++
      Seq(probeBInput, probeCInput, largestTokenBox, zeroAmountBox) ++ hugeBoxes, Some(emissionBox), ctxAtTip(s), s)
    us.stateContext.currentHeight shouldBe tip
    us.stateContext.currentParameters.maxBlockCost shouldBe 1500000
    us
  }

  private def upcoming(us: UtxoState): UpcomingStateContext =
    us.stateContext.upcoming(defaultMinerPk.value, defaultTimestamp, devnet.chainSettings.initialNBits,
      Array.fill(3)(0: Byte), ErgoValidationSettingsUpdate.empty, YoloLaunchParameters.blockVersion)

  private def upcomingCtx(s: ErgoSettings): UpcomingStateContext =
    ctxAtTip(s).upcoming(defaultMinerPk.value, defaultTimestamp, devnet.chainSettings.initialNBits,
      Array.fill(3)(0: Byte), ErgoValidationSettingsUpdate.empty, YoloLaunchParameters.blockVersion)

  /** The YOLO emission tx for `next` (built with the switch on: a switch-off state is handed the same transaction) */
  private def emissionTx(us: UtxoState): ErgoTransaction =
    CandidateGenerator.collectRewards(Some(emissionBox), tip, Seq.empty, defaultMinerPk, ctxAtTip(devnet)).head

  private def rentClaim(): ErgoTransaction = StorageRentClaimBuilder.buildClaim(rentBoxes, next, YoloLaunchParameters,
    defaultMinerPk, None, Set.empty).get

  /**
    * Spends `inputs` (`true` script) into a change output, a fee output paying `fee` (none if 0) and `pad` further
    * outputs of `PadValue` each (100 cost each, to tune the cost)
    */
  private def payTx(inputs: Seq[ErgoBox], fee: Long = Fee, pad: Int = 0): ErgoTransaction = {
    val change = new ErgoBoxCandidate(inputs.map(_.value).sum - fee - pad * PadValue, TrueTree, next)
    val feeOut = if (fee > 0) Seq(new ErgoBoxCandidate(fee, feeProp, next)) else Seq.empty
    val padOuts = Seq.fill(pad)(new ErgoBoxCandidate(PadValue, TrueTree, next))
    ErgoTransaction(inputs.map(b => new Input(b.id, ProverResult.empty)).toIndexedSeq, IndexedSeq.empty,
      (change +: (feeOut ++ padOuts)).toIndexedSeq)
  }

  private def costOf(us: UtxoState, ctx: ErgoStateContext, tx: ErgoTransaction): Int =
    us.validateWithCost(tx, ctx, BlockCost, None).get

  /**
    * A pool transaction from `boxes` whose cost is at most `target` and within one output cost (100) of it: inputs
    * first (each `true` input adds a fixed cost), then padding outputs
    */
  private def txAtCost(us: UtxoState, ctx: ErgoStateContext, boxes: IndexedSeq[ErgoBox], target: Int): ErgoTransaction = {
    val c1 = costOf(us, ctx, payTx(boxes.take(1)))
    val perInput = costOf(us, ctx, payTx(boxes.take(2))) - c1
    val n = (target - (c1 - perInput)) / perInput
    n should be <= boxes.size
    val withInputs = costOf(us, ctx, payTx(boxes.take(n)))
    val outputCost = ctx.currentParameters.outputCost
    val pad = (target - withInputs) / outputCost
    val tx = payTx(boxes.take(n), pad = pad)
    val cost = costOf(us, ctx, tx)
    cost should be <= target
    cost should be > target - outputCost
    tx
  }

  /** Cost of the fee transaction collecting the fee outputs of `txs`, as collectTxs validates it */
  private def feeTxOf(ctx: ErgoStateContext, txs: Seq[ErgoTransaction]): (ErgoTransaction, Int) = {
    val feeTx = CandidateGenerator.collectFees(tip, txs, defaultMinerPk, ctx).get
    val boxes = feeTx.inputs.map(i => txs.flatMap(_.outputs).find(b => java.util.Arrays.equals(b.id, i.boxId)).get)
    feeTx -> feeTx.statefulValidity(boxes, IndexedSeq.empty, ctx)(
      org.ergoplatform.wallet.interpreter.ErgoInterpreter(ctx.currentParameters)).get
  }

  private def emptyHistoryStorage(s: ErgoSettings): HistoryStorage =
    HistoryStorage(s.copy(directory = createTempDir.getAbsolutePath))

  /** History reader with `parent` as the best full block, and `storage` (the extra index) for the rent scan */
  private def historyWithBest(parent: Header,
                              anyTx: ErgoTransaction,
                              s: ErgoSettings,
                              storage: HistoryStorage): ErgoHistoryReader = {
    val genesisId: ModifierId = bytesToId(Array.fill(32)(9: Byte))
    val parentExtension = Extension(parent.id, NipopowAlgos.packInterlinks(Seq(genesisId)))
    val parentBlock = ErgoFullBlock(parent, BlockTransactions(parent.id, parent.version, Seq(anyTx)), parentExtension, None)
    new ErgoHistoryReader {
      override val historyStorage: HistoryStorage = storage
      override protected val settings: ErgoSettings = s
      override protected def requireProofs: Boolean = false
      override protected def process(m: NonHeaderBlockSection): Try[ProgressInfo[BlockSection]] = ???
      override protected def validate(m: NonHeaderBlockSection): Try[Unit] = ???
      override val powScheme: AutolykosPowScheme = s.chainSettings.powScheme

      override def bestFullBlockOpt: Option[ErgoFullBlock] = Some(parentBlock)
      override def fullBlockHeight: Int = parent.height
      override def typedModifierById[T <: BlockSection : ClassTag](id: ModifierId): Option[T] =
        Some(parentExtension).collect { case e: T => e }
      override def requiredDifficultyAfter(parent: Header): BigInt =
        DifficultySerializer.decodeCompactBits(s.chainSettings.initialNBits)
    }
  }

  /** Extra index synced at `tip`, holding `rent` and their storage rent eligibility entries, in that order */
  private def indexedHistory(us: UtxoState, s: ErgoSettings, anyTx: ErgoTransaction,
                             rent: Seq[ErgoBox]): ErgoHistoryReader = {
    val storage = emptyHistoryStorage(s)
    val entries: Seq[ExtraIndex] = rent.zipWithIndex.flatMap { case (b, i) =>
      val globalIndex = 100L + i
      val iEb = new IndexedErgoBox(b.creationHeight, None, None, None, b, globalIndex)
      Seq(iEb, NumericBoxIndex(globalIndex, bytesToId(b.id)), StorageRentBox(iEb))
    }
    storage.insertExtra(Array(ExtraIndexer.IndexedHeightKey -> ByteBuffer.allocate(4).putInt(tip).array),
      entries.toArray)
    historyWithBest(us.stateContext.lastHeaderOpt.get, anyTx, s, storage)
  }

  /** Storage rent eligibility entries left in the extra index of `h` */
  private def rentEntries(h: ErgoHistoryReader): Int = h.storageRentBoxesAtOrBefore(Int.MaxValue, 1000).length

  /**
    * Candidate made by `createCandidate` with `s`, the emission transaction, `prioritized` and `pool` as mempool
    * transactions, with an extra index holding `rent`; and the history, to read the index afterwards
    */
  private def candidateWith(us: UtxoState, s: ErgoSettings, pool: Seq[ErgoTransaction], rent: Seq[ErgoBox],
                            prioritized: Seq[ErgoTransaction] = Seq.empty)
  : (CandidateBlock, Seq[ModifierId], ErgoHistoryReader) = {
    val emission = emissionTx(us)
    val h = indexedHistory(us, s, emission, rent)
    rentEntries(h) shouldBe rent.size
    val (c, eliminate) = CandidateGenerator.createCandidate(defaultMinerPk, h,
      ErgoValidationSettingsUpdate.empty, us, pool.map(tx => UnconfirmedTransaction(tx, None)), Some(emission),
      prioritized, s).get
    val block = c.candidateBlock
    block.transactions.head.id shouldBe emission.id
    (block, eliminate.ids, h)
  }

  /** Candidate made by `createCandidate` with `s`, the emission transaction and `pool` as mempool transactions */
  private def candidate(us: UtxoState, s: ErgoSettings, pool: Seq[ErgoTransaction]): (CandidateBlock, Seq[ModifierId]) = {
    val (block, eliminated, _) = candidateWith(us, s, pool, rentBoxes)
    block -> eliminated
  }

  private def isClaimOf(boxes: Seq[ErgoBox])(tx: ErgoTransaction): Boolean =
    tx.inputs.exists(i => boxes.exists(b => java.util.Arrays.equals(b.id, i.boxId)))

  private def isRentClaim(tx: ErgoTransaction): Boolean = isClaimOf(rentBoxes)(tx)

  /** The candidate, proven with the test PoW scheme, applied to `us`: it must be a valid block */
  private def applyCandidate(us: UtxoState, block: CandidateBlock): UtxoState = {
    val fb = powScheme.proveCandidate(block, defaultMinerSecretNumber).get
    fb.header.height shouldBe next
    val applied = us.applyModifier(fb, None)(_ => ())
    applied shouldBe a[Success[_]]
    applied.get
  }

  /** Upper bound of a claim's cost as `StorageRentClaimBuilder` keeps it: base, plus each input with its output */
  private def claimBound(claim: ErgoTransaction, params: org.ergoplatform.settings.Parameters): Long = {
    val inputs = claim.inputs.map(i => allOldBoxes.find(b => java.util.Arrays.equals(b.id, i.boxId)).get)
    StorageRentClaimBuilder.claimBaseCostBound(params) + inputs.zipWithIndex.map { case (b, k) =>
      StorageRentClaimBuilder.boxCostBound(b, claim.outputCandidates(k), params)
    }.sum
  }

  private lazy val allOldBoxes: Seq[ErgoBox] =
    rentBoxes ++ tokenRentBoxes ++ windowBoxes ++ burnedBoxes ++ mixedBoxes ++ sharedIdBoxes ++
      Seq(largestTokenBox, zeroAmountBox) ++ hugeBoxes

  property("measured: a pool tx at the relay limit is included beside the emission tx, a 100-box rent claim built " +
    "by StorageRentClaimBuilder and the fee tx, in a candidate built with the YOLO launch parameters; the block " +
    "applies") {
    relayLimit shouldBe 1000000
    Budget shouldBe 1350000

    val us = stateAtTip()
    val ctx = upcoming(us)
    ctx.currentHeight shouldBe next
    ctx.currentParameters.maxBlockCost shouldBe BlockCost

    // the pieces, measured one by one as collectTxs validates them
    val emission = emissionTx(us)
    val claim = rentClaim()
    claim.inputs.size shouldBe 100
    claim.outputs.size shouldBe 101 // 100 recreations and the rent fee output
    val atLimit = txAtCost(us, ctx, forAtLimit, relayLimit)
    val emissionCost = costOf(us, ctx, emission)
    val claimCost = costOf(us, ctx, claim)
    val atLimitCost = costOf(us, ctx, atLimit)
    val (_, feeTxCost) = feeTxOf(ctx, Seq(emission, claim, atLimit))
    val mandatory = emissionCost + claimCost + feeTxCost
    val room = Budget - mandatory
    info(s"YOLO launch parameters: block cost limit $BlockCost, safe gap ${BlockCost - Budget}, budget $Budget")
    info(s"emission tx $emissionCost, 100-box rent claim $claimCost (${claim.inputs.size} inputs, " +
      s"${claim.outputs.size} outputs), fee tx $feeTxCost: $mandatory together")
    info(s"room left for one pool tx: $room; pool tx at the relay limit $relayLimit costs $atLimitCost " +
      s"(${atLimit.inputs.size} inputs, ${atLimit.outputs.size} outputs, ${atLimit.size} bytes); spare " +
      s"${room - atLimitCost}")
    // the same claim over boxes carrying tokens of their own costs 400 more per token (each is read in the input and
    // in the recreated output, once as an access and once as a distinct token): not covered by the reserve's margin
    // beyond about 120 tokens in all
    val tokenClaim = StorageRentClaimBuilder.buildClaim(tokenRentBoxes, next, YoloLaunchParameters, defaultMinerPk,
      None, Set.empty).get
    tokenClaim.inputs.size shouldBe 100
    val tokenClaimCost = costOf(us, ctx, tokenClaim)
    tokenClaimCost shouldBe claimCost + 100 * TokensPerBox * 400
    info(s"100-box rent claim with $TokensPerBox tokens per box: $tokenClaimCost")

    // the claim cost budget: a token-free 100-box claim is built whole under it, and its cost bound is exact
    CandidateGenerator.YoloRentClaimCostBudget shouldBe 270000
    val budgeted = StorageRentClaimBuilder.buildClaim(rentBoxes, next, YoloLaunchParameters, defaultMinerPk, None,
      Set.empty, maxCost = Some(CandidateGenerator.YoloRentClaimCostBudget.toLong)).get
    budgeted.id shouldBe claim.id
    claimBound(claim, ctx.currentParameters) shouldBe claimCost
    emissionCost should be <= CandidateGenerator.YoloEmissionTxCostAllowance
    feeTxCost should be <= CandidateGenerator.YoloFeeTxCostAllowance

    // the reserve behind the start-up warning covers the measured mandatory transactions
    mandatory should be <= CandidateGenerator.YoloMandatoryTxsCostReserve
    room should be >= relayLimit
    CandidateGenerator.yoloRelayLimitWarning(devnet) shouldBe None

    // the candidate: emission, the node's own claim (from the extra index), the pool tx, the fee tx
    val (block, eliminated) = candidate(us, devnet, Seq(atLimit))
    eliminated shouldBe empty
    block.transactions.size shouldBe 4
    val ownClaim = block.transactions(1)
    isRentClaim(ownClaim) shouldBe true
    ownClaim.inputs.size shouldBe 100
    ownClaim.id shouldBe claim.id
    block.transactions(2).id shouldBe atLimit.id
    block.transactions(3).inputs.map(i => bytesToId(i.boxId)) shouldBe IndexedSeq(bytesToId(atLimit.outputs(1).id))

    // the candidate, proven with the test PoW scheme, is a valid block (rule 308 attestation included)
    applyCandidate(us, block).boxById(atLimit.outputs.head.id) shouldBe defined
  }

  property("rent claim cost bound: never below the measured cost, for boxes without tokens, with many distinct " +
    "tokens, with shared ids, recreated and burned, with salvaged tokens; a budget stops the claim at the first box " +
    "past it, always admits the first box, and reports nothing as permanently skipped") {
    val us = stateAtTip()
    val ctx = upcoming(us)
    val params = ctx.currentParameters
    devnet.nodeSettings.storageRentTokenWhitelist shouldBe empty
    val salvage: Set[ModifierId] = burnedIds.map(_.toModifierId).toSet
    val sets: Seq[(String, Seq[ErgoBox], Set[ModifierId])] = Seq(
      ("no tokens", rentBoxes, Set.empty),
      ("10 distinct tokens per box", tokenRentBoxes, Set.empty),
      ("28 distinct tokens per box", windowBoxes, Set.empty),
      ("the same 28 ids in every box, recreated", sharedIdBoxes, Set.empty),
      ("the same 111 ids in every box, burned", burnedBoxes, Set.empty),
      ("token-free and 57 distinct tokens, interleaved", mixedBoxes, Set.empty),
      ("the same 111 ids, burned with the ids salvaged", burnedBoxes.take(20), salvage))
    val budgets: Seq[Option[Long]] =
      Seq(None, Some(CandidateGenerator.YoloRentClaimCostBudget.toLong), Some(100000L), Some(30000L), Some(0L))
    sets.foreach { case (name, boxes, whitelist) =>
      // the reference copy of the v6.0.8 builder: the node's builder without a budget must give exactly this claim
      val whole = UpstreamV608Assembly.buildClaim(boxes, next, params, defaultMinerPk, None, whitelist).get
      budgets.foreach { budget =>
        withClue(s"$name, budget $budget: ") {
          val skipped = mutable.ArrayBuffer.empty[ErgoBox]
          val claim = StorageRentClaimBuilder.buildClaim(boxes, next, params, defaultMinerPk, None, whitelist,
            b => skipped += b, budget).get
          // boxes left out for the budget are not permanently unclaimable
          skipped shouldBe empty
          val bound = claimBound(claim, params)
          val cost = costOf(us, ctx, claim)
          bound should be >= cost.toLong
          // the claimed boxes are the first ones of the unbudgeted claim
          claim.inputs.map(i => bytesToId(i.boxId)) shouldBe whole.inputs.take(claim.inputs.size).map(i => bytesToId(i.boxId))
          budget.foreach { b =>
            if (claim.inputs.size > 1) bound should be <= b else claim.inputs.size shouldBe 1
            if (claim.inputs.size < whole.inputs.size) {
              // stopped at the first box that would have taken the bound past the budget
              val k = claim.inputs.size
              val nextBox = allOldBoxes.find(x => java.util.Arrays.equals(x.id, whole.inputs(k).boxId)).get
              bound + StorageRentClaimBuilder.boxCostBound(nextBox, whole.outputCandidates(k), params) should be > b
            }
          }
          if (budget.isEmpty) claim.id shouldBe whole.id
          if (budget.contains(0L)) claim.inputs.size shouldBe 1
          info(s"$name, budget ${budget.getOrElse("none")}: ${claim.inputs.size} boxes, cost $cost, bound $bound")
        }
      }
    }
    // the largest one-box bound stays under the budget, so admitting the first box whatever the budget is safe: the
    // box with the most tokens that fits in 4,096 bytes (one more token does not fit), claimed alone under a budget
    // of 0, recreated with its tokens
    val big = largestTokenBox
    val n = big.additionalTokens.size
    big.bytes.length should be <= ErgoBox.MaxBoxSize
    oldBox(0x7a000000, n + 1, (0 until n + 1).map(j => tokenId(0x7a000000, 0, j))).bytes.length should be >
      ErgoBox.MaxBoxSize
    val skipped = mutable.ArrayBuffer.empty[ErgoBox]
    val one = StorageRentClaimBuilder.buildClaim(Seq(big), next, params, defaultMinerPk, None, Set.empty,
      b => skipped += b, maxCost = Some(0L)).get
    skipped shouldBe empty
    one.inputs.map(i => bytesToId(i.boxId)) shouldBe Seq(bytesToId(big.id))
    val recreatedBig = one.outputCandidates.head
    recreatedBig.additionalTokens.size shouldBe n
    val bigBound = StorageRentClaimBuilder.claimBaseCostBound(params) +
      StorageRentClaimBuilder.boxCostBound(big, recreatedBig, params)
    val bigCost = costOf(us, ctx, one)
    bigCost.toLong should be <= bigBound
    bigBound should be <= CandidateGenerator.YoloRentClaimCostBudget.toLong
    info(s"largest token box: $n tokens, ${big.bytes.length} bytes; one-box claim cost $bigCost, bound $bigBound")
  }

  property("rent claim builder without a budget gives exactly upstream v6.0.8's claim and the same permanently " +
    "unclaimable boxes (reference copy), over sets with unclaimable boxes, salvaged tokens and dropped recreations") {
    val params = upcoming(stateAtTip()).currentParameters
    val reemissionId = tokenId(0x7d000000, 0, 0)
    // value at or below the minimum allowed value: never claimable
    val belowMin = oldBox(0x7d000000, 1, Seq.empty, 1000L)
    // carries the re-emission token (with a re-emission token id given): never claimable
    val withReemission = new ErgoBox(BoxValue, FalseTree, Colls.fromItems(reemissionId -> 1L), Map.empty,
      boxId(0x7d000000, 2), 0, next - StoragePeriod - 10)
    // burned with salvaged tokens, whose P2PK proceeds output is larger than the box and above its value's dust
    // floor: never claimable
    val salvageIds = (0 until 10).map(j => tokenId(0x7d000001, 0, j))
    val dustyBurn = {
      // a value of one above the minimum allowed value, at the box's own size (the value's VLQ length settles)
      def withValue(v: Long): ErgoBox = oldBox(0x7d000000, 3, salvageIds, v)
      val v = Iterator.iterate(1L)(v => params.minValuePerByte.toLong * withValue(v).bytes.length + 1).drop(3).next()
      val box = withValue(v)
      box.value shouldBe params.minValuePerByte.toLong * box.bytes.length + 1
      box
    }
    // too young: transient, not reported
    val young = boxAt(FalseTree, next - StoragePeriod + 5, seed = 77)
    val salvage: Set[ModifierId] = (burnedIds ++ salvageIds).map(_.toModifierId).toSet
    // a storage fee of 1 per byte: recreation fees far below the fee output's dust floor, so recreations are dropped
    val lowFee = new org.ergoplatform.settings.Parameters(params.height,
      params.parametersTable.updated(org.ergoplatform.settings.Parameters.StorageFeeFactorIncrease, 1),
      params.proposedUpdate)
    def ids(bs: ErgoBox*): Seq[ModifierId] = bs.map(b => bytesToId(b.id))
    type Check = (Option[ErgoTransaction], Seq[ModifierId]) => Unit
    val sets: Seq[(String, Seq[ErgoBox], Option[ModifierId], Set[ModifierId], org.ergoplatform.settings.Parameters,
      Check)] = Seq(
      ("unclaimable boxes among claimable ones", Seq(belowMin, rentBoxes(0), withReemission, young, dustyBurn) ++
        rentBoxes.slice(1, 5), Some(reemissionId.toModifierId), salvage, params, { (claim, skipped) =>
        skipped shouldBe ids(belowMin, withReemission, dustyBurn)
        claim.get.inputs.map(i => bytesToId(i.boxId)) shouldBe ids(rentBoxes.take(5): _*)
      }),
      ("the same, no re-emission id given", Seq(belowMin, rentBoxes(0), withReemission, young, dustyBurn) ++
        rentBoxes.slice(1, 5), None, Set.empty, params, { (claim, skipped) =>
        skipped shouldBe ids(belowMin)
        claim.get.inputs.size shouldBe 7
      }),
      ("burned with salvaged tokens, and an unclaimable salvage", burnedBoxes.take(10) :+ dustyBurn, None, salvage,
        params, { (claim, skipped) =>
        skipped shouldBe ids(dustyBurn)
        claim.get.outputCandidates.take(10).foreach(_.additionalTokens.size shouldBe burnedIds.size)
      }),
      ("recreations dropped for dusty fees, with unclaimable boxes", rentBoxes.take(5) ++ Seq(belowMin, young), None,
        Set.empty, lowFee, { (claim, skipped) =>
        skipped shouldBe ids(belowMin)
        claim shouldBe None
      }),
      ("more claimable boxes than the cap, with unclaimable ones in between", rentBoxes.take(60) ++
        Seq(belowMin, dustyBurn) ++ tokenRentBoxes.take(60), None, salvage, params, { (claim, skipped) =>
        skipped shouldBe ids(belowMin, dustyBurn)
        claim.get.inputs.size shouldBe StorageRentClaimBuilder.MaxClaims
      }))
    sets.foreach { case (name, boxes, reemission, whitelist, ps, check) =>
      withClue(s"$name: ") {
        val upSkipped = mutable.ArrayBuffer.empty[ModifierId]
        val nodeSkipped = mutable.ArrayBuffer.empty[ModifierId]
        val up = UpstreamV608Assembly.buildClaim(boxes, next, ps, defaultMinerPk, reemission, whitelist,
          b => upSkipped += bytesToId(b.id))
        val node = StorageRentClaimBuilder.buildClaim(boxes, next, ps, defaultMinerPk, reemission, whitelist,
          b => nodeSkipped += bytesToId(b.id))
        node.map(_.id) shouldBe up.map(_.id)
        nodeSkipped shouldBe upSkipped
        check(node, nodeSkipped.toSeq)
        info(s"$name: claim of ${node.map(_.inputs.size).getOrElse(0)} boxes, ${nodeSkipped.size} permanently " +
          "unclaimable")
      }
    }
  }

  property("rent claims over token-heavy boxes, switch on: the candidate has the pool txs (one at the relay limit), the " +
    "claim within its cost budget, nothing eliminated, the rent index intact, and applies; switch off: upstream's " +
    "stall at the emission tx, and upstream's eviction of all 100 boxes") {
    val atLimit = {
      val us = stateAtTip()
      txAtCost(us, upcoming(us), forAtLimit, relayLimit)
    }
    val small = payTx(forSmall.slice(0, 1))
    val pool = Seq(atLimit, small)
    val budget = CandidateGenerator.YoloRentClaimCostBudget
    Seq(("stall window, 100 recreated boxes of 28 distinct tokens", windowBoxes, 1345100),
      ("stall window, 100 burned boxes holding the same 111 ids", burnedBoxes, 1346100),
      ("above the budget, 50 token-free and 50 boxes of 57 tokens", mixedBoxes, 1365100)).foreach {
      case (name, boxes, upstreamCost) =>
        withClue(s"$name: ") {
          // fresh states: applying a candidate moves the state on
          val us = stateAtTip()
          val ctx = upcoming(us)
          val usOff = stateAtTip(switchOff)
          // upstream's claim over these boxes (reference copy of the v6.0.8 builder) and its measured cost
          val upClaim = UpstreamV608Assembly.buildClaim(boxes, next, ctx.currentParameters, defaultMinerPk, None,
            Set.empty).get
          upClaim.inputs.size shouldBe 100
          costOf(us, ctx, upClaim) shouldBe upstreamCost

          // switch on
          val (on, onEliminated, onHistory) = candidateWith(us, devnet, pool, boxes)
          onEliminated shouldBe empty
          on.transactions.size shouldBe 5
          val claim = on.transactions(1)
          isClaimOf(boxes)(claim) shouldBe true
          claim.inputs.size should (be > 1 and be < 100)
          val claimCost = costOf(us, ctx, claim)
          claimCost should be <= budget
          on.transactions.slice(2, 4).map(_.id) shouldBe Seq(atLimit.id, small.id)
          rentEntries(onHistory) shouldBe 100
          applyCandidate(us, on)
          info(s"$name: switch on, claim of ${claim.inputs.size} boxes costing $claimCost (bound " +
            s"${claimBound(claim, ctx.currentParameters)}), pool tx at the relay limit and a small one included")

          // switch off, as upstream: the same claim the reference builder makes, and upstream's outcome
          val (off, offEliminated, offHistory) = candidateWith(usOff, switchOff, pool, boxes)
          if (upstreamCost <= Budget) {
            // passes validation but overflows beside the emission tx, cannot be skipped: emission tx only
            off.transactions.map(_.id) shouldBe Seq(emissionTx(us).id)
            offEliminated shouldBe empty
            rentEntries(offHistory) shouldBe 100
          } else {
            // over the budget: reported invalid, and all 100 boxes evicted from the index, the honest ones too
            offEliminated shouldBe Seq(upClaim.id)
            rentEntries(offHistory) shouldBe 0
            off.transactions.slice(1, 3).map(_.id) shouldBe Seq(atLimit.id, small.id)
          }
        }
    }
  }

  property("switch on: fee outputs carrying many tokens never end assembly (two txs paying fee " +
    "outputs of 60 tokens of 2^62, then a small one): all three are in, the second fee box is left unspent, the " +
    "block applies, and a later block's miner can collect that box; switch off: upstream's stop, pinned") {
    val us = stateAtTip()
    val txA = tokenFeeTx(forTokenFee(0))
    val txB = tokenFeeTx(forTokenFee(1))
    val small = payTx(forSmall.slice(0, 1))
    val feeA = txA.outputs(1)
    val feeB = txB.outputs(1)
    feeA.bytes.length should be > 2500

    val (on, onEliminated, _) = candidateWith(us, devnet, Seq(txA, txB, small), rentBoxes)
    onEliminated shouldBe empty
    on.transactions.size shouldBe 6
    on.transactions.slice(2, 5).map(_.id) shouldBe Seq(txA.id, txB.id, small.id)
    val feeTx = on.transactions.last
    // fee boxes are taken by box id (ErgoState.newBoxes, as upstream): one of the two token fee boxes, and small's
    val feeInputs = feeTx.inputs.map(i => bytesToId(i.boxId))
    feeInputs.size shouldBe 2
    feeInputs should contain (bytesToId(small.outputs(1).id))
    val (taken, left) = if (feeInputs.contains(bytesToId(feeA.id))) feeA -> feeB else feeB -> feeA
    feeInputs should contain (bytesToId(taken.id))
    feeInputs should not contain bytesToId(left.id)
    val minerOut = feeTx.outputs.head
    minerOut.value shouldBe 2 * Fee
    // nothing burned: every token of the collected fee box, with its full amount
    minerOut.additionalTokens.toArray.toSeq.map { case (id, amt) => id.toModifierId -> amt } shouldBe
      taken.additionalTokens.toArray.toSeq.map { case (id, amt) => id.toModifierId -> amt }
    val applied = applyCandidate(us, on)
    applied.boxById(taken.id) shouldBe empty
    applied.boxById(left.id) shouldBe defined

    // the fee script lets the miner of any later block collect the box left over
    val later = applied.stateContext.upcoming(defaultMinerPk.value, defaultTimestamp + 30000,
      devnet.chainSettings.initialNBits, Array.fill(3)(0: Byte), ErgoValidationSettingsUpdate.empty,
      YoloLaunchParameters.blockVersion)
    later.currentHeight shouldBe next + 1
    val minerProp = ErgoTreePredef.rewardOutputScript(devnet.chainSettings.monetary.minerRewardDelay, defaultMinerPk)
    val collectLater = ErgoTransaction(IndexedSeq(new Input(left.id, ProverResult.empty)), IndexedSeq.empty,
      IndexedSeq(new ErgoBoxCandidate(left.value, minerProp, later.currentHeight, left.additionalTokens)))
    applied.validateWithCost(collectLater, later, BlockCost, None) shouldBe a[Success[_]]

    // switch off: the fee tx for txA and txB is invalid (box size), so assembly ends after txA, as upstream does
    val usOff = stateAtTip(switchOff)
    val (off, offEliminated, _) = candidateWith(usOff, switchOff, Seq(txA, txB, small), rentBoxes)
    offEliminated shouldBe empty
    off.transactions.size shouldBe 4
    off.transactions(2).id shouldBe txA.id
    val ctxOff = upcoming(usOff)
    UpstreamV608Assembly.collectTxs(defaultMinerPk, Budget, ctxOff.currentParameters.maxBlockSize, usOff, ctxOff,
      Seq(emissionTx(us), rentClaim(), txA, txB, small))._1.map(_.id) shouldBe off.transactions.map(_.id)
  }

  property("fee collection, switch on: fee boxes are taken in order while the miner output stays valid (at most 100 " +
    "token entries, ids merged, no amount overflow, box size and dust), the others are left out; every token of a " +
    "collected box is in the miner output with its full amount; upstream's 100-entry cut is never reached; the " +
    "choice for the first boxes does not change as boxes are added; switch off: upstream's fee tx, pinned") {
    val on = upcomingCtx(devnet)
    val off = upcomingCtx(switchOff)
    on.chainSettings.yolo.enabled shouldBe true
    off.chainSettings.yolo.enabled shouldBe false
    val verifier = ErgoInterpreter(on.currentParameters)

    var seed = 0
    // one tx per fee box, paying it as its only output (collectFees reads the outputs only)
    def payer(tokens: Seq[(ErgoBox.TokenId, Long)], value: Long = Fee): ErgoTransaction = {
      seed += 1
      ErgoTransaction(IndexedSeq(new Input(forSmall(250 + seed).id, ProverResult.empty)), IndexedSeq.empty,
        IndexedSeq(new ErgoBoxCandidate(value, feeProp, next, Colls.fromItems(tokens: _*))))
    }
    def ids(prefix: Int, n: Int, amount: Long): Seq[(ErgoBox.TokenId, Long)] =
      (0 until n).map(j => tokenId(prefix, 0, j) -> amount)
    val x = tokenId(0x77000005, 0, 0)

    // one tx per fee box; fee boxes are taken by box id (ErgoState.newBoxes, as upstream), so each case states what
    // holds in any order
    val c1 = Seq(payer(ids(0x77000000, 60, BigAmount)), payer(ids(0x77000001, 60, BigAmount)), payer(Seq.empty))
    val c2 = Seq(payer(ids(0x77000002, 60, 5)), payer(ids(0x77000003, 40, 5)), payer(ids(0x77000004, 1, 5)),
      payer(Seq(tokenId(0x77000002, 0, 7) -> 3L)), payer(Seq.empty))
    val c3 = Seq(payer(Seq(x -> (Long.MaxValue - 1))), payer(Seq(x -> 2L)), payer(Seq.empty))
    val c4 = Seq(payer(ids(0x77000006, 60, 1L << 61)), payer(ids(0x77000006, 60, 1L << 61)))
    val cases: Seq[(String, Seq[ErgoTransaction], Seq[ErgoBox] => Unit)] = Seq(
      ("two boxes of 60 tokens of 2^62 (together 120 entries, over 4,096 bytes), then a token-free one", c1, { got =>
        got.size shouldBe 2
        got should contain (c1(2).outputs.head)
      }),
      ("60, then 40 new ids, then a new id (101 entries with them), then an id already in, then token-free", c2, { got =>
        got should contain (c2(4).outputs.head)
        Seq(0, 1, 2).forall(k => got.contains(c2(k).outputs.head)) shouldBe false
        got.size should be >= 3
      }),
      ("two amounts of one id that would pass Long.MaxValue together", c3, { got =>
        got.size shouldBe 2
        got should contain (c3(2).outputs.head)
      }),
      ("the same 60 ids twice, merged", c4, { got =>
        got.size shouldBe 2
      }))

    cases.foreach { case (name, txs, expect) =>
      withClue(s"$name: ") {
        val feeBoxes = txs.map(_.outputs.head)
        val feeTx = CandidateGenerator.collectFees(tip, txs, defaultMinerPk, on).get
        val collected = feeTx.inputs.map(i => feeBoxes.find(b => java.util.Arrays.equals(b.id, i.boxId)).get)
        expect(collected)
        // in the order upstream takes the fee boxes
        collected.map(b => bytesToId(b.id)) shouldBe
          ErgoState.newBoxes(txs).filter(collected.contains).map(b => bytesToId(b.id))
        // a valid transaction: its output is a valid box
        feeTx.statefulValidity(collected.toIndexedSeq, IndexedSeq.empty, on)(verifier) shouldBe a[Success[_]]
        val minerOut = feeTx.outputs.head
        minerOut.bytes.length should be <= ErgoBox.MaxBoxSize
        minerOut.value shouldBe collected.map(_.value).sum
        // token conservation, merged by id, nothing burned
        def merged(boxes: Seq[ErgoBox]): Map[ModifierId, BigInt] = boxes.flatMap(_.additionalTokens.toArray.toSeq)
          .groupBy(_._1.toModifierId).map { case (k, v) => k -> v.map(t => BigInt(t._2)).sum }
        val outTokens = minerOut.additionalTokens.toArray.toSeq.map { case (id, amt) => id.toModifierId -> BigInt(amt) }
        outTokens.map(_._1).distinct.size shouldBe outTokens.size
        outTokens.toMap shouldBe merged(collected)
        // upstream's take(100) never cuts: at most 100 entries before it
        outTokens.size should be <= 100
        // each fee box left out would not fit beside the collected ones: too many entries, an amount past
        // Long.MaxValue, or more than 4,096 bytes
        feeBoxes.filterNot(collected.contains).foreach { b =>
          val m = merged(collected :+ b)
          val tooBig = m.size <= 100 && m.values.forall(_ <= Long.MaxValue) && {
            val box = new ErgoBoxCandidate(minerOut.value + b.value, minerOut.ergoTree, next,
              Colls.fromItems(m.toSeq.map { case (k, v) => (Digest32Coll @@ Colls.fromArray(scorex.util.idToBytes(k))) -> v.toLong }: _*))
            box.toBox(bytesToId(Array.fill(32)(0: Byte)), 0).bytes.length > ErgoBox.MaxBoxSize
          }
          (m.size > 100 || m.values.exists(_ > Long.MaxValue) || tooBig) shouldBe true
        }
        // for every part of the pool, the fee tx is built and valid
        (1 until txs.size).foreach { k =>
          val part = txs.take(k)
          val partTx = CandidateGenerator.collectFees(tip, part, defaultMinerPk, on).get
          val partBoxes = partTx.inputs.map(i => feeBoxes.find(b => java.util.Arrays.equals(b.id, i.boxId)).get)
          partTx.statefulValidity(partBoxes.toIndexedSeq, IndexedSeq.empty, on)(verifier) shouldBe a[Success[_]]
        }
        // switch off: exactly upstream's fee tx (reference copy)
        CandidateGenerator.collectFees(tip, txs, defaultMinerPk, off).map(_.id) shouldBe
          UpstreamV608Assembly.collectFees(tip, txs, defaultMinerPk, off).map(_.id)
        info(s"$name: collected ${collected.size} of ${feeBoxes.size}, miner output ${minerOut.bytes.length} bytes, " +
          s"${outTokens.size} token entries")
      }
    }
    // upstream cuts at 100 entries in the second case: it takes all five boxes and burns the last two entries
    val upstreamTx = UpstreamV608Assembly.collectFees(tip, c2, defaultMinerPk, off).get
    upstreamTx.inputs.size shouldBe 5
    upstreamTx.outputs.head.additionalTokens.size shouldBe 100
  }


  /** Spends 9 one-coin `true` boxes into a change output and `k` fee outputs of 0.01 each */
  private def manyFeesTx(k: Int): ErgoTransaction = {
    val inputs = forSmall.slice(150, 159)
    val change = new ErgoBoxCandidate(inputs.map(_.value).sum - k * Fee, TrueTree, next)
    ErgoTransaction(inputs.map(b => new Input(b.id, ProverResult.empty)), IndexedSeq.empty,
      (change +: Seq.fill(k)(new ErgoBoxCandidate(Fee, feeProp, next))).toIndexedSeq)
  }

  /** The fee tx collecting the fee outputs of `txs` (switch as `ctx` says), and its validation as collectTxs runs it */
  private def feeTxTry(ctx: ErgoStateContext, txs: Seq[ErgoTransaction]): (ErgoTransaction, Seq[ErgoBox], Try[Int]) = {
    val feeTx = CandidateGenerator.collectFees(tip, txs, defaultMinerPk, ctx).get
    val outputs = txs.flatMap(_.outputs)
    val boxes = feeTx.inputs.map(i => outputs.find(b => java.util.Arrays.equals(b.id, i.boxId)).get)
    (feeTx, boxes, feeTx.statefulValidity(boxes, IndexedSeq.empty, ctx)(ErgoInterpreter(ctx.currentParameters)))
  }

  private def mempoolProcess(s: ErgoSettings, us: UtxoState, tx: ErgoTransaction): (ErgoMemPool, ProcessingOutcome) =
    ErgoMemPool.empty(s).process(UnconfirmedTransaction(tx, None), us)

  /** Declined by the YOLO fee output policy, with a message containing `text` */
  private def refusedWith(o: ProcessingOutcome, text: String): Boolean = o match {
    case d: ProcessingOutcome.Declined =>
      d.e.isInstanceOf[YoloFeeOutputPolicy.Refusal] && d.e.getMessage.contains(text)
    case _ => false
  }

  private def isRefusal(t: Try[_]): Boolean = t match {
    case Failure(_: YoloFeeOutputPolicy.Refusal) => true
    case _ => false
  }

  property("fee collection cost estimate (YoloFeeOutputPolicy): exactly derived from the fee tx's validation cost and " +
    "never below it, for 1 to 713 token-free fee boxes and for fee boxes with distinct, shared and mixed tokens, " +
    "switch on and off; 713 fee boxes give a valid fee tx, 714 an invalid one") {
    val on = upcomingCtx(devnet)
    val off = upcomingCtx(switchOff)
    val params = on.currentParameters
    val cs = devnet.chainSettings
    val base = YoloFeeOutputPolicy.feeTxBaseCost(params)
    base shouldBe (ErgoInterpreter.interpreterInitCost + params.outputCost).toLong
    base shouldBe 10100L
    YoloFeeOutputPolicy.feeBoxCollectionCost(new ErgoBoxCandidate(Fee, feeProp, next), params) shouldBe
      (params.inputCost + YoloFeeOutputPolicy.FeeScriptCostBound).toLong
    params.inputCost shouldBe 2000
    params.tokenAccessCost shouldBe 100

    /** The fee tx's cost as ErgoTransaction.validateStateful charges it, with the fee script costing `script` */
    def exact(boxes: Seq[ErgoBox], minerOut: ErgoBoxCandidate, script: Long): Long = {
      val inTokens = boxes.flatMap(_.additionalTokens.toArray.toSeq.map(_._1.toModifierId))
      val outTokens = minerOut.additionalTokens.toArray.toSeq.map(_._1.toModifierId)
      base + boxes.size * (params.inputCost + script) + params.tokenAccessCost.toLong *
        (inTokens.size + inTokens.distinct.size + outTokens.size + outTokens.distinct.size)
    }

    // token-free: the fee script costs the same for every input
    val scriptCosts = Seq(1, 2, 10, 100, 713).map { k =>
      val tx = manyFeesTx(k)
      val estimate = base + YoloFeeOutputPolicy.feeCollectionCost(tx, cs, params)
      estimate shouldBe base + k * 2100L
      Seq(on, off).map { ctx =>
        val (_, boxes, cost) = feeTxTry(ctx, Seq(tx))
        boxes.size shouldBe k
        val c = cost.get.toLong
        c should be <= estimate
        (c - base - k * params.inputCost) % k shouldBe 0
        (c - base - k * params.inputCost) / k
      }.distinct
    }.distinct
    scriptCosts.size shouldBe 1
    scriptCosts.head.size shouldBe 1
    val script = scriptCosts.head.head
    script should be <= YoloFeeOutputPolicy.FeeScriptCostBound.toLong
    info(s"fee script: $script per fee box (bound ${YoloFeeOutputPolicy.FeeScriptCostBound}); fee tx base $base")

    // with tokens: one tx per fee box (collectFees reads the outputs only)
    def payer(n: Int, tokens: Seq[(ErgoBox.TokenId, Long)]): ErgoTransaction =
      ErgoTransaction(IndexedSeq(new Input(forSmall(270 + n).id, ProverResult.empty)), IndexedSeq.empty,
        IndexedSeq(new ErgoBoxCandidate(Fee, feeProp, next, Colls.fromItems(tokens: _*))))
    def ids(prefix: Int, i: Int, n: Int): Seq[(ErgoBox.TokenId, Long)] = (0 until n).map(j => tokenId(prefix, i, j) -> 5L)
    val sets: Seq[(String, Seq[ErgoTransaction])] = Seq(
      ("three boxes of 30 distinct tokens", (0 until 3).map(i => payer(i, ids(0x7e000000, i, 30)))),
      ("three boxes of the same 30 ids", (0 until 3).map(i => payer(3 + i, ids(0x7e000001, 0, 30)))),
      ("two token-free, two of the same 20 ids, one of 50 distinct", Seq(payer(6, Seq.empty), payer(7, Seq.empty),
        payer(8, ids(0x7e000002, 0, 20)), payer(9, ids(0x7e000002, 0, 20)), payer(10, ids(0x7e000003, 0, 50)))),
      ("one box of 100 distinct tokens", Seq(payer(11, ids(0x7e000004, 0, 100)))))
    sets.foreach { case (name, txs) =>
      withClue(s"$name: ") {
        val estimate = base + txs.map(YoloFeeOutputPolicy.feeCollectionCost(_, cs, params)).sum
        // switch on (the policy's case): every box collected, tokens merged by id
        val (feeTx, boxes, cost) = feeTxTry(on, txs)
        boxes.size shouldBe txs.size
        cost.get.toLong shouldBe exact(boxes, feeTx.outputCandidates.head, script)
        cost.get.toLong should be <= estimate
        info(s"$name: fee tx ${cost.get}, estimate $estimate")
      }
    }

    // thresholds for the fee tx itself: valid with 713 fee boxes, invalid with 714
    Seq(on, off).foreach { ctx =>
      feeTxTry(ctx, Seq(manyFeesTx(713)))._3 shouldBe a[Success[_]]
      feeTxTry(ctx, Seq(manyFeesTx(714)))._3.isFailure shouldBe true
    }
  }

  property("switch on: the mempool, the cleanup's revalidation and the API's check (one function) refuse a tx whose " +
    "own cost plus what its fee outputs add to the fee tx is above the relay limit (9 inputs, " +
    "730 fee outputs of 0.01), with the boundary covered on both sides and the thresholds (601 to 713, 714 " +
    "and more) refused; switch off: all accepted, as upstream") {
    val us = stateAtTip()
    val usOff = stateAtTip(switchOff)
    val ctx = us.stateContext.simplifiedUpcoming()
    val ctxOff = usOff.stateContext.simplifiedUpcoming()
    val params = ctx.currentParameters
    def cost(tx: ErgoTransaction): Int = us.validateWithCost(tx, ctx, relayLimit, None).get

    val probe = manyFeesTx(730)
    cost(probe) shouldBe 101109
    YoloFeeOutputPolicy.feeCollectionCost(probe, devnet.chainSettings, params) shouldBe 730 * 2100L
    info(s"probe: ${probe.outputs.size} outputs, ${probe.size} bytes, cost ${cost(probe)}, fee collection cost " +
      s"${730 * 2100}: ${cost(probe) + 730 * 2100} against the relay limit $relayLimit")

    // the tx's own cost grows by one output cost per fee output, the estimate by 2,100: the largest K accepted
    val c1 = cost(manyFeesTx(1))
    val d = cost(manyFeesTx(2)) - c1
    d shouldBe params.outputCost
    val kMax = (relayLimit - c1 + d) / (d + 2100)
    cost(manyFeesTx(kMax)) + 2100L * kMax should be <= relayLimit.toLong
    cost(manyFeesTx(kMax + 1)) + 2100L * (kMax + 1) should be > relayLimit.toLong
    info(s"largest K accepted with the switch on: $kMax")

    Seq(1, kMax, kMax + 1, 600, 601, 713, 714, 730).foreach { k =>
      withClue(s"K = $k: ") {
        val tx = manyFeesTx(k)
        val (poolOn, outcomeOn) = mempoolProcess(devnet, us, tx)
        val (poolOff, outcomeOff) = mempoolProcess(switchOff, usOff, tx)
        val cleanupOn = CleanupWorker.revalidate(us, tx, ctx, relayLimit)
        val cleanupOff = CleanupWorker.revalidate(usOff, tx, ctxOff, relayLimit)
        // the API's check (ErgoBaseApiRoute.verifyTransaction) calls the same function, against the pool's state
        val apiOn = YoloFeeOutputPolicy.validateWithCost(us.withMempool(ErgoMemPool.empty(devnet)), tx, ctx, relayLimit)
        outcomeOff shouldBe a[ProcessingOutcome.Accepted]
        poolOff.size shouldBe 1
        cleanupOff shouldBe Success(cost(tx))
        if (k <= kMax) {
          outcomeOn shouldBe a[ProcessingOutcome.Accepted]
          poolOn.size shouldBe 1
          cleanupOn shouldBe Success(cost(tx))
          apiOn shouldBe Success(cost(tx))
        } else {
          refusedWith(outcomeOn, s"fee output(s) add up to ${2100L * k} to the miner's fee transaction") shouldBe true
          poolOn.size shouldBe 0
          poolOn.isInvalidated(tx.id) shouldBe true
          isRefusal(cleanupOn) shouldBe true
          isRefusal(apiOn) shouldBe true
        }
      }
    }
  }

  property("switch on: a sender's two token fee boxes (ground to sort first by id, making the " +
    "miner output exactly 4,096 bytes) do not push out the token-free fee boxes: all five honest fee boxes are " +
    "collected, nothing is burned and the block applies; switch off: upstream's stop, pinned") {
    val us = stateAtTip()
    val usOff = stateAtTip(switchOff)
    val ctxOff = upcoming(usOff)
    val params = upcoming(us).currentParameters
    probeBTokens.size shouldBe 100
    val (ids0, ids1) = probeBTokens.splitAt(50)
    val fee1 = new ErgoBoxCandidate(1000000L, feeProp, next, Colls.fromItems(ids0: _*))
    val fee2 = new ErgoBoxCandidate(ProbeBValue - 1000000L, feeProp, next, Colls.fromItems(ids1: _*))
    Seq(fee1, fee2).foreach { f =>
      sizeOf(f) should be <= 2196
      f.value should be >= sizeOf(f).toLong * params.minValuePerByte
    }
    def attacker(r: Long): ErgoTransaction = ErgoTransaction(IndexedSeq(new Input(probeBInput.id, ProverResult.empty)),
      IndexedSeq.empty, IndexedSeq(fee1, fee2,
        new ErgoBoxCandidate(Coin - ProbeBValue, TrueTree, next, Colls.emptyColl, Map(ErgoBox.R4 -> LongConstant(r)))))
    val honest = (0 until 5).map(i => payTx(forSmall.slice(10 + i, 11 + i)))
    def isFeeBox(b: ErgoBox): Boolean = java.util.Arrays.equals(b.propositionBytes, feeProp.bytes)
    def senderIds(tx: ErgoTransaction): Set[ModifierId] = tx.outputs.take(2).map(b => bytesToId(b.id)).toSet
    val (attack, tries) = Iterator.from(0).take(100000).map(r => attacker(r.toLong) -> r).find { case (tx, _) =>
      ErgoState.newBoxes(tx +: honest).filter(isFeeBox).take(2).map(b => bytesToId(b.id)).toSet == senderIds(tx)
    }.get
    info(s"the sender's fee boxes sort first after $tries tries")

    // the sender's two fee boxes alone: collected together into a miner output of exactly 4,096 bytes
    val alone = CandidateGenerator.yoloFeeCollection(attack.outputs.take(2), minerRewardProp, next, params).get
    alone._1.size shouldBe 2
    sizeOf(alone._2) shouldBe ErgoBox.MaxBoxSize
    alone._2.value shouldBe ProbeBValue

    val (on, onEliminated, _) = candidateWith(us, devnet, attack +: honest, rentBoxes)
    onEliminated shouldBe empty
    on.transactions.size shouldBe 9
    on.transactions.slice(2, 8).map(_.id) shouldBe (attack +: honest).map(_.id)
    val feeTx = on.transactions.last
    val feeInputs = feeTx.inputs.map(i => bytesToId(i.boxId))
    val honestFeeIds = honest.map(t => bytesToId(t.outputs(1).id))
    honestFeeIds.foreach(id => feeInputs should contain (id))
    // one of the sender's boxes fits beside them; both would take the output past 4,096 bytes
    feeInputs.toSet.intersect(senderIds(attack)).size shouldBe 1
    feeInputs.size shouldBe 6
    // spent in the order of ErgoState.newBoxes, as upstream
    val feeBoxes = ErgoState.newBoxes(on.transactions.slice(0, 8)).filter(isFeeBox)
    feeInputs shouldBe feeBoxes.map(b => bytesToId(b.id)).filter(feeInputs.contains)
    val collected = feeInputs.map(id => feeBoxes.find(b => bytesToId(b.id) == id).get)
    val minerOut = feeTx.outputs.head
    minerOut.value shouldBe collected.map(_.value).sum
    minerOut.value should be > 5 * Fee
    // nothing burned: the miner output holds exactly the tokens of the collected boxes
    def merged(boxes: Seq[ErgoBox]): Map[ModifierId, BigInt] = boxes.flatMap(_.additionalTokens.toArray.toSeq)
      .groupBy(_._1.toModifierId).map { case (k, v) => k -> v.map(t => BigInt(t._2)).sum }
    minerOut.additionalTokens.toArray.toSeq.map { case (id, amt) => id.toModifierId -> BigInt(amt) }.toMap shouldBe
      merged(collected)
    sizeOf(minerOut) should be <= ErgoBox.MaxBoxSize
    val applied = applyCandidate(us, on)
    honest.foreach(t => applied.boxById(t.outputs(1).id) shouldBe empty)
    attack.outputs.take(2).count(b => applied.boxById(b.id).isDefined) shouldBe 1

    // switch off: upstream collects the sender's two boxes (4,096 bytes), then the first honest box makes the fee
    // tx invalid and assembly ends
    val (off, offEliminated, _) = candidateWith(usOff, switchOff, attack +: honest, rentBoxes)
    offEliminated shouldBe empty
    off.transactions.map(_.id) shouldBe UpstreamV608Assembly.collectTxs(defaultMinerPk, Budget,
      ctxOff.currentParameters.maxBlockSize, usOff, ctxOff, Seq(emissionTx(us), rentClaim(), attack) ++ honest)._1
      .map(_.id)
    off.transactions.size shouldBe 4
    off.transactions.last.outputs.head.value shouldBe ProbeBValue
  }

  property("switch on: the mempool, the cleanup's revalidation and the API's check refuse a tx with a fee output " +
    "carrying tokens (0.5 coin and 101 tokens, which no stock node collects; and a single " +
    "token); switch off: accepted, as upstream") {
    val us = stateAtTip()
    val usOff = stateAtTip(switchOff)
    val ctx = us.stateContext.simplifiedUpcoming()
    val ctxOff = usOff.stateContext.simplifiedUpcoming()
    val params = ctx.currentParameters
    val probeC = ErgoTransaction(IndexedSeq(new Input(probeCInput.id, ProverResult.empty)), IndexedSeq.empty,
      IndexedSeq(new ErgoBoxCandidate(Coin / 2, TrueTree, next),
        new ErgoBoxCandidate(Coin / 2, feeProp, next, probeCInput.additionalTokens)))
    // a fee box no stock node collects: 101 token entries cannot go in one miner output of at most 100
    val probeCFee = probeC.outputs(1)
    info(s"101-token fee box: ${probeCFee.bytes.length} bytes, ${probeCFee.additionalTokens.size} tokens")
    CandidateGenerator.yoloFeeCollection(Seq(probeCFee), minerRewardProp, next, params) shouldBe None
    val tokens = probeCInput.additionalTokens.toArray.toSeq
    val oneToken = ErgoTransaction(IndexedSeq(new Input(probeCInput.id, ProverResult.empty)), IndexedSeq.empty,
      IndexedSeq(new ErgoBoxCandidate(Coin - Fee, TrueTree, next, Colls.fromItems(tokens.drop(1): _*)),
        new ErgoBoxCandidate(Fee, feeProp, next, Colls.fromItems(tokens.take(1): _*))))
    Seq((probeC, 101), (oneToken, 1)).foreach { case (tx, n) =>
      withClue(s"fee output with $n token(s): ") {
        val cost = us.validateWithCost(tx, ctx, relayLimit, None).get
        val (poolOn, on) = mempoolProcess(devnet, us, tx)
        refusedWith(on, s"fee output #1 carries $n token(s)") shouldBe true
        poolOn.size shouldBe 0
        poolOn.isInvalidated(tx.id) shouldBe true
        isRefusal(CleanupWorker.revalidate(us, tx, ctx, relayLimit)) shouldBe true
        isRefusal(YoloFeeOutputPolicy.validateWithCost(us.withMempool(ErgoMemPool.empty(devnet)), tx, ctx,
          relayLimit)) shouldBe true
        isRefusal(YoloFeeOutputPolicy.checkTokens(tx, devnet.chainSettings)) shouldBe true

        val (poolOff, off) = mempoolProcess(switchOff, usOff, tx)
        off shouldBe a[ProcessingOutcome.Accepted]
        poolOff.size shouldBe 1
        CleanupWorker.revalidate(usOff, tx, ctxOff, relayLimit) shouldBe Success(cost)
        YoloFeeOutputPolicy.checkTokens(tx, switchOff.chainSettings) shouldBe Success(())
      }
    }
    // a token-free fee output is not refused for tokens
    YoloFeeOutputPolicy.checkTokens(payTx(forSmall.slice(0, 1)), devnet.chainSettings) shouldBe Success(())
  }

  property("switch on: a rent claim that loses a race (a box spent by a tx placed before it) evicts nothing from the " +
    "rent index and the other boxes stay claimable; a claim failing validation evicts only the box that fails on its " +
    "own, or every input when none fails alone; switch off: every input evicted, as upstream") {
    val small = payTx(forSmall.slice(0, 1))
    val claim = rentClaim()

    // the race: another miner's one-box claim of the first rent box, placed as a prioritized tx
    val us = stateAtTip()
    val ctx = upcoming(us)
    val competitor = StorageRentClaimBuilder.buildClaim(rentBoxes.take(1), next, ctx.currentParameters,
      defaultMinerPk, None, Set.empty).get
    val (on, onEliminated, onHistory) = candidateWith(us, devnet, Seq(small), rentBoxes, Seq(competitor))
    onEliminated shouldBe Seq(claim.id)
    on.transactions.map(_.id).take(3) shouldBe Seq(emissionTx(us).id, competitor.id, small.id)
    rentEntries(onHistory) shouldBe 100
    // after the block, the first box is gone from the state (the indexer drops its row when it indexes the block,
    // and the scan does not resolve it meanwhile) and the other 99 make a valid claim at the next height
    val applied = applyCandidate(us, on)
    val resolved = rentBoxes.flatMap(b => applied.boxById(b.id))
    resolved.size shouldBe 99
    val later = applied.stateContext.upcoming(defaultMinerPk.value, defaultTimestamp + 30000,
      devnet.chainSettings.initialNBits, Array.fill(3)(0: Byte), ErgoValidationSettingsUpdate.empty,
      YoloLaunchParameters.blockVersion)
    val again = StorageRentClaimBuilder.buildClaim(resolved, later.currentHeight, later.currentParameters,
      defaultMinerPk, None, Set.empty, maxCost = Some(CandidateGenerator.YoloRentClaimCostBudget.toLong)).get
    again.inputs.size shouldBe 99
    applied.validateWithCost(again, later, BlockCost, None) shouldBe a[Success[_]]

    val usOff = stateAtTip(switchOff)
    val (_, offEliminated, offHistory) = candidateWith(usOff, switchOff, Seq(small), rentBoxes, Seq(competitor))
    offEliminated shouldBe Seq(claim.id)
    rentEntries(offHistory) shouldBe 0

    // a genuine failure caused by one box: a token of amount 0, which the builder does not check
    val withBad = rentBoxes.take(99) :+ zeroAmountBox
    val badClaim = StorageRentClaimBuilder.buildClaim(withBad, next, ctx.currentParameters, defaultMinerPk, None,
      Set.empty).get
    badClaim.inputs.size shouldBe 100
    stateAtTip().validateWithCost(badClaim, ctx, BlockCost, None).isFailure shouldBe true
    def indexedIds(h: ErgoHistoryReader): Seq[ModifierId] = h.storageRentBoxesAtOrBefore(Int.MaxValue, 1000).toSeq
      .map(e => NumericBoxIndex.getBoxByNumber(h, e.globalIndex).get.id)
    val (bad, badEliminated, badHistory) = candidateWith(stateAtTip(), devnet, Seq(small), withBad)
    badEliminated shouldBe Seq(badClaim.id)
    bad.transactions.map(_.id) should contain (small.id)
    indexedIds(badHistory) shouldBe rentBoxes.take(99).map(b => bytesToId(b.id))
    val (_, badOffEliminated, badOffHistory) = candidateWith(stateAtTip(switchOff), switchOff, Seq(small), withBad)
    badOffEliminated shouldBe Seq(badClaim.id)
    rentEntries(badOffHistory) shouldBe 0

    // a genuine failure no single box causes: two boxes of 2^62 overflow the input sum together; each is claimable
    // alone, so every input is evicted, as upstream, and the same claim is not built again
    val withHuge = hugeBoxes ++ rentBoxes.take(10)
    hugeBoxes.foreach { b =>
      val alone = StorageRentClaimBuilder.buildClaim(Seq(b), next, ctx.currentParameters, defaultMinerPk, None,
        Set.empty).get
      stateAtTip().validateWithCost(alone, ctx, BlockCost, None) shouldBe a[Success[_]]
    }
    val (_, hugeEliminated, hugeHistory) = candidateWith(stateAtTip(), devnet, Seq(small), withHuge)
    hugeEliminated.size shouldBe 1
    rentEntries(hugeHistory) shouldBe 0
  }

}
