package org.ergoplatform.mining

import ch.qos.logback.classic.{Level, Logger => LogbackLogger}
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.AppenderBase
import org.ergoplatform.consensus.ProgressInfo
import org.ergoplatform.mining.YoloAssemblyPolicy.{Outcome, PoolTx}
import org.ergoplatform.mining.difficulty.DifficultySerializer
import org.ergoplatform.modifiers.history.BlockTransactions
import org.ergoplatform.modifiers.history.extension.Extension
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.history.popow.NipopowAlgos
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction, UnsignedErgoTransaction}
import org.ergoplatform.modifiers.{BlockSection, ErgoFullBlock, NonHeaderBlockSection}
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.EliminateTransactions
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.nodeView.history.extra.{ExtraIndex, ExtraIndexer, IndexedErgoBox, NumericBoxIndex, StorageRentBox}
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.mempool.ErgoMemPoolUtils.ProcessingOutcome
import org.ergoplatform.nodeView.mempool.{ErgoMemPool, YoloFeeOutputPolicy, YoloRefusedBoxes, YoloRefusedBoxesStore}
import org.ergoplatform.nodeView.state.{ErgoState, ErgoStateContext, UpcomingStateContext, UtxoState, VotingData}
import org.ergoplatform.settings.Constants.{FalseTree, TrueTree}
import org.ergoplatform.settings.{Args, ErgoSettings, ErgoSettingsReader, ErgoValidationSettingsUpdate, NetworkType,
  Parameters, YoloLaunchParameters}
import org.ergoplatform.utils.{ErgoCompilerHelpers, ErgoCorePropertyTest, StorageRentTestHelpers}
import org.ergoplatform.wallet.interpreter.ErgoInterpreter
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, ErgoTreePredef, Input, UnsignedInput}
import org.slf4j.LoggerFactory
import scorex.util.{ModifierId, bytesToId}
import sigma.Colls
import sigma.ast.{ByteArrayConstant, ErgoTree, IntConstant}
import sigma.data.Digest32Coll
import sigma.exceptions.CostLimitException
import sigma.interpreter.{ContextExtension, ProverResult}

import java.io.File
import java.nio.ByteBuffer
import scala.collection.mutable
import scala.reflect.ClassTag
import scala.util.{Success, Try}

/**
  * The YOLO block assembly policy (`YoloAssemblyPolicy`) with the YOLO launch parameters (block cost
  * limit 1,500,000, budget B = 1,350,000) for a rent-collecting miner. Properties checked:
  *  - work bound: work on transactions left out of a candidate stays within Z, tier by tier, and within L each;
  *    at most 3 passes per parent end short of Z;
  *  - no stop by valid transactions that do not fit: they are passed over at zero work;
  *  - censorship has a price: each lie uses up a box, which is refused until spent on chain (sponsor rule);
  *  - room for the mandatory transactions by construction (claim size and fee boxes, relay limit in use);
  *  - switch off is upstream.
  * Also the four shapes of "over L" and a transaction whose script branches on the pre-header.
  */
class YoloAssemblyPolicySpec extends ErgoCorePropertyTest with StorageRentTestHelpers with ErgoCompilerHelpers {
  import org.ergoplatform.utils.ErgoCoreTestConstants.{defaultMinerPk, defaultMinerSecretNumber, defaultProver,
    defaultTimestamp, genesisStateDigest, powScheme, validationSettingsNoIl}
  import org.ergoplatform.utils.generators.ErgoCoreGenerators.defaultHeaderGen

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
  private lazy val relayLimit: Int = devnet.nodeSettings.maxTransactionCost
  private val BlockCost: Int = YoloLaunchParameters.maxBlockCost
  private val Budget: Int = BlockCost - CandidateGenerator.safeGap(BlockCost)

  private lazy val emissionBox: ErgoBox = {
    val e = devnet.chainSettings.yolo.emission
    val genesis = ErgoState.genesisBoxes(devnet.chainSettings)
    val nft = (Digest32Coll @@ Colls.fromArray(e.emissionNftIdBytes)) -> 1L
    new ErgoBoxCandidate(devnet.chainSettings.yoloEmissionRulesOpt.get.boxValueAt(next), e.emissionTree, tip,
      Colls.fromItems(nft), genesis.head.additionalRegisters).toBox(bytesToId(Array.fill(32)(7: Byte)), 0)
  }

  private def boxId(prefix: Int, i: Int): ModifierId = bytesToId(ByteBuffer.allocate(32).putInt(prefix).putInt(i).array)

  private def tokenId(prefix: Int, i: Int): ErgoBox.TokenId =
    Digest32Coll @@ Colls.fromArray(ByteBuffer.allocate(32).putInt(prefix).putInt(i).putInt(0x5a5a5a5a).array)

  /** A box at `tip - 10` (young: not rent-eligible) */
  private def box(prefix: Int, i: Int, tree: ErgoTree = TrueTree, value: Long = Coin,
                  regs: Map[ErgoBox.NonMandatoryRegisterId, sigma.ast.EvaluatedValue[_ <: sigma.ast.SType]] = Map.empty,
                  height: Int = tip - 10): ErgoBox =
    new ErgoBox(value, tree, Colls.emptyColl, regs, boxId(prefix, i), 0, height)

  private def boxes(prefix: Int, n: Int, from: Int = 0): IndexedSeq[ErgoBox] = (from until from + n).map(box(prefix, _))

  /** 100 boxes past the storage period, `false` script, for the miner's own rent claim (225,100 measured) */
  private lazy val rentBoxes: Seq[ErgoBox] =
    (0 until StorageRentClaimBuilder.MaxClaims).map(i => box(0x60000000, i, FalseTree, BoxValue, height = next - StoragePeriod - 10))

  /** As `rentBoxes`, with 10 tokens each: the claim is stopped by its 270,000 cost budget */
  private lazy val tokenRentBoxes: Seq[ErgoBox] = (0 until StorageRentClaimBuilder.MaxClaims).map { i =>
    val tokens = (0 until 10).map(j => tokenId(0x61000000 + i, j) -> 1L)
    new ErgoBox(BoxValue, FalseTree, Colls.fromItems(tokens: _*), Map.empty, boxId(0x61000000, i), 0,
      next - StoragePeriod - 10)
  }

  // ------------------------------------------------------------------------------- pre-header branching W

  /** W's script: cheap when the pre-header's miner key is the group generator (the mempool's context) */
  private lazy val wScript: ErgoTree = compileSourceV5(
    """{
      |  val big = SELF.R4[Coll[Byte]].get
      |  val n = SELF.R5[Int].get
      |  if (CONTEXT.preHeader.minerPk == groupGenerator) sigmaProp(true)
      |  else sigmaProp(big.slice(0, n).fold(0, { (acc: Int, b: Byte) => acc + blake2b256(Coll(b) ++ big).size }) > 0)
      |}""".stripMargin, 0)

  /** W's tuning: 9 inputs of R4 = 3,800 bytes and R5 = 3,493 cost 28,497 at relay and 1,339,905 at assembly */
  private val WInputs = 9
  private val WN = 3493

  private def wBox(i: Int): ErgoBox = box(0x7f000000, i, wScript, regs = Map(
    ErgoBox.R4 -> ByteArrayConstant(Array.fill(3800)(i.toByte)), ErgoBox.R5 -> IntConstant(WN)))

  private lazy val wBoxes: IndexedSeq[ErgoBox] = (0 until 30 * WInputs).map(wBox)

  /** The k-th W: spends its 9 W boxes into a change output and a 0.01 fee output; `salt` re-makes it */
  private def wTx(k: Int, salt: Long = 0, extra: Seq[Input] = Seq.empty, extraValue: Long = 0): ErgoTransaction = {
    val bs = wBoxes.slice(WInputs * k, WInputs * (k + 1))
    ErgoTransaction((bs.map(b => new Input(b.id, ProverResult.empty)) ++ extra).toIndexedSeq, IndexedSeq.empty,
      IndexedSeq(new ErgoBoxCandidate(bs.map(_.value).sum + extraValue - Fee - salt, TrueTree, next),
        new ErgoBoxCandidate(Fee + salt, feeProp, next)))
  }

  // ---------------------------------------------------------------------------------------------- states, txs

  /** One parent header for every state of this spec, so that a candidate made on one state applies to a fresh one */
  private lazy val tipHeader: Header =
    defaultHeaderGen.sample.get.copy(version = YoloLaunchParameters.blockVersion, height = tip)

  private def ctxAtTip(s: ErgoSettings, params: Parameters = YoloLaunchParameters): ErgoStateContext =
    new ErgoStateContext(Seq(tipHeader), None, genesisStateDigest, params, validationSettingsNoIl,
      VotingData.empty)(s.chainSettings)

  /** UTXO state at `tip` with the launch parameters, the emission box, both rent box sets, the W boxes and `extra` */
  private def stateAt(extra: Seq[ErgoBox], s: ErgoSettings = devnet, params: Parameters = YoloLaunchParameters,
                      withW: Boolean = false): UtxoState = {
    val us = utxoStateAt(Seq(emissionBox) ++ rentBoxes ++ tokenRentBoxes ++ (if (withW) wBoxes else Seq.empty) ++ extra,
      Some(emissionBox), ctxAtTip(s, params), s)
    us.stateContext.currentHeight shouldBe tip
    us
  }

  private def upcoming(us: UtxoState): UpcomingStateContext =
    us.stateContext.upcoming(defaultMinerPk.value, defaultTimestamp, devnet.chainSettings.initialNBits,
      Array.fill(3)(0: Byte), ErgoValidationSettingsUpdate.empty, YoloLaunchParameters.blockVersion)

  private def key(us: UtxoState): Int = YoloAssemblyPolicy.costKey(upcoming(us).currentParameters)

  private def emissionTx: ErgoTransaction =
    CandidateGenerator.collectRewards(Some(emissionBox), tip, Seq.empty, defaultMinerPk, ctxAtTip(devnet)).head

  /** Spends `inputs` into a change output (`changeTree`), a fee output paying `fee` (none if 0) and `outs` extras */
  private def payTx(inputs: Seq[ErgoBox], fee: Long = Fee, outs: Seq[ErgoBoxCandidate] = Seq.empty): ErgoTransaction = {
    val change = new ErgoBoxCandidate(inputs.map(_.value).sum - fee - outs.map(_.value).sum, TrueTree, next)
    val feeOut = if (fee > 0) Seq(new ErgoBoxCandidate(fee, feeProp, next)) else Seq.empty
    ErgoTransaction(inputs.map(b => new Input(b.id, ProverResult.empty)).toIndexedSeq, IndexedSeq.empty,
      (change +: (feeOut ++ outs)).toIndexedSeq)
  }

  private def costOf(us: UtxoState, ctx: ErgoStateContext, tx: ErgoTransaction): Int =
    us.validateWithCost(tx, ctx, BlockCost, None).get

  /** Cost in the mempool's context (`simplifiedUpcoming`), as admission records it */
  private def relayCost(us: UtxoState, tx: ErgoTransaction): Int =
    us.validateWithCost(tx, us.stateContext.simplifiedUpcoming(), relayLimit, None).get

  /** A pool transaction carrying the record `r` under record key `k` */
  private def recorded(tx: ErgoTransaction, r: Int, k: Int): UnconfirmedTransaction = {
    val now = System.currentTimeMillis()
    new UnconfirmedTransaction(tx, Some(r), now, now, Some(tx.bytes), None, Some(k))
  }

  /** `txs` put through mempool admission one by one (all accepted), with the settings `s` */
  private def admitted(s: ErgoSettings, us: UtxoState, txs: Seq[ErgoTransaction]): ErgoMemPool =
    txs.foldLeft(ErgoMemPool.empty(s)) { case (pool, tx) =>
      val (p, outcome) = pool.process(UnconfirmedTransaction(tx, None), us)
      withClue(s"admission of ${tx.id}: ") { outcome shouldBe a[ProcessingOutcome.Accepted] }
      p
    }

  /** A pool tx paying a fee with `n` `true` inputs from `bs`, whose cost is at most `target` and close to it */
  private def txAtCost(us: UtxoState, ctx: ErgoStateContext, bs: IndexedSeq[ErgoBox], target: Int): ErgoTransaction = {
    val c1 = costOf(us, ctx, payTx(bs.take(1)))
    val perInput = costOf(us, ctx, payTx(bs.take(2))) - c1
    val n = (target - (c1 - perInput)) / perInput
    n should be <= bs.size
    val withInputs = costOf(us, ctx, payTx(bs.take(n)))
    val pad = (target - withInputs) / ctx.currentParameters.outputCost
    val tx = payTx(bs.take(n), outs = Seq.fill(pad)(new ErgoBoxCandidate(Coin / 1000, TrueTree, next)))
    costOf(us, ctx, tx) should be <= target
    tx
  }

  // ---------------------------------------------------------------------------------------------- history, candidates

  private def historyWithBest(parent: Header, anyTx: ErgoTransaction, s: ErgoSettings,
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

  /** Extra index synced at `tip`, holding `rent` and their storage rent eligibility entries */
  private def indexedHistory(us: UtxoState, s: ErgoSettings, rent: Seq[ErgoBox]): ErgoHistoryReader = {
    val storage = HistoryStorage(s.copy(directory = createTempDir.getAbsolutePath))
    val entries: Seq[ExtraIndex] = rent.zipWithIndex.flatMap { case (b, i) =>
      val globalIndex = 100L + i
      val iEb = new IndexedErgoBox(b.creationHeight, None, None, None, b, globalIndex)
      Seq(iEb, NumericBoxIndex(globalIndex, bytesToId(b.id)), StorageRentBox(iEb))
    }
    storage.insertExtra(Array(ExtraIndexer.IndexedHeightKey -> ByteBuffer.allocate(4).putInt(tip).array),
      entries.toArray)
    historyWithBest(us.stateContext.lastHeaderOpt.get, emissionTx, s, storage)
  }

  /** Candidate from `createCandidate` with the emission tx, an extra index of `rent` and `pool`, in that order */
  private def candidateWith(us: UtxoState, s: ErgoSettings, pool: Seq[UnconfirmedTransaction],
                            rent: Seq[ErgoBox] = rentBoxes,
                            memory: YoloAssemblyMemory = new YoloAssemblyMemory,
                            history: Option[ErgoHistoryReader] = None): (CandidateBlock, EliminateTransactions) = {
    val h = history.getOrElse(indexedHistory(us, s, rent))
    val (c, eliminate) = CandidateGenerator.createCandidate(defaultMinerPk, h, ErgoValidationSettingsUpdate.empty, us,
      pool, Some(emissionTx), Seq.empty, s, memory).get
    c.candidateBlock.transactions.head.id shouldBe emissionTx.id
    c.candidateBlock -> eliminate
  }

  private def ids(block: CandidateBlock): Seq[ModifierId] = block.transactions.map(_.id)

  private def inputIds(txs: ErgoTransaction*): Seq[ModifierId] = txs.flatMap(_.inputs.map(i => bytesToId(i.boxId)))

  private def applyCandidate(us: UtxoState, block: CandidateBlock): UtxoState = {
    val fb = powScheme.proveCandidate(block, defaultMinerSecretNumber).get
    val applied = us.applyModifier(fb, None)(_ => ())
    applied shouldBe a[Success[_]]
    applied.get
  }

  /** One pass of the YOLO loop, with the emission tx (and `mandatory`) first, at the launch budget */
  private def collect(us: UtxoState, pool: Seq[PoolTx], mandatory: Seq[ErgoTransaction] = Seq.empty,
                      excluded: Set[ModifierId] = Set.empty): YoloAssemblyPolicy.Result = {
    val ctx = upcoming(us)
    YoloAssemblyPolicy.collect(defaultMinerPk, us, ctx, YoloAssemblyPolicy.budget(ctx.currentParameters),
      YoloAssemblyPolicy.relayLimitInUse(relayLimit, ctx.currentParameters), emissionTx +: mandatory, Set.empty, pool,
      excluded)
  }

  /** WARN lines of `Tx verification failed` written by `ErgoTransaction.verifyInput` while `f` runs */
  private def verifyInputWarnings[T](f: => T): (T, Int) = {
    val logger = LoggerFactory.getLogger("org.ergoplatform.modifiers.mempool").asInstanceOf[LogbackLogger]
    var count = 0
    val appender = new AppenderBase[ILoggingEvent] {
      override def append(e: ILoggingEvent): Unit =
        if (e.getLevel.isGreaterOrEqual(Level.WARN) && e.getFormattedMessage.contains("verification")) count += 1
    }
    appender.setContext(logger.getLoggerContext)
    appender.start()
    logger.addAppender(appender)
    try {
      val res = f
      res -> count
    } finally {
      logger.detachAppender(appender)
    }
  }

  /** Validation as the YOLO loop runs it at abort limit `limit`: the result and the failure the verifier kept */
  private def atLimit(us: UtxoState, tx: ErgoTransaction, limit: Int): (Try[Int], Option[Throwable]) = {
    val ctx = upcoming(us)
    val verifier = new YoloAssemblyPolicy.YoloAssemblyVerifier(ctx.currentParameters)
    val atL = ctx.copy(currentParameters = ctx.currentParameters.withBlockCost(limit))(ctx.chainSettings)
    val res = tx.statefulValidity(tx.inputs.flatMap(i => us.boxById(i.boxId)), IndexedSeq.empty, atL, 0L)(verifier)
    res -> verifier.failure
  }

  // ========================================================================================= over L

  property("the four shapes of \"over L\" are told apart from genuine failures, and the assembly verifier " +
    "writes no WARN line from verifyInput") {
    val tokenBox = new ErgoBox(Coin, TrueTree, Colls.fromItems((0 until 100).map(j => tokenId(0x62000000, j) -> 1L): _*),
      Map.empty, boxId(0x62000000, 0), 0, tip - 10)
    val oldBox = box(0x63000000, 0, FalseTree, BoxValue, height = next - StoragePeriod - 10)
    val youngFalse = box(0x64000000, 0, FalseTree)
    val throwing = box(0x65000000, 0, compileSourceV5("sigmaProp(SELF.R4[Int].get > 0)", 0))
    val many = boxes(0x66000000, 25)
    val us = stateAt(Seq(tokenBox, oldBox, youngFalse, throwing) ++ many, withW = true)
    val params = upcoming(us).currentParameters

    // (a) CostLimitException inside a script: W at its abort limit r + slack(r)
    val w = wTx(0)
    val r = relayCost(us, w)
    val lw = r + YoloAssemblyPolicy.slack(r)
    info(s"W: relay cost $r, abort limit $lw")
    val ((wRes, wKept), wWarnings) = verifyInputWarnings(atLimit(us, w, lw))
    wRes.isFailure shouldBe true
    wKept.exists(_.isInstanceOf[CostLimitException]) shouldBe true
    YoloAssemblyPolicy.isOverLimit(wRes.failed.get, wKept) shouldBe true
    wWarnings shouldBe 0
    // the plain verifier logs the same failure at WARN (the capture works)
    val (_, plainWarnings) = verifyInputWarnings {
      val ctx = upcoming(us)
      val atL = ctx.copy(currentParameters = ctx.currentParameters.withBlockCost(lw))(ctx.chainSettings)
      w.statefulValidity(w.inputs.flatMap(i => us.boxById(i.boxId)), IndexedSeq.empty, atL, 0L)(
        ErgoInterpreter(ctx.currentParameters))
    }
    plainWarnings should be > 0

    // (b) initial cost: 25 inputs at L = 49,999
    val (initRes, initKept) = atLimit(us, payTx(many), 49999)
    initRes.failed.get.getMessage should include(": initial cost")
    initKept shouldBe None
    YoloAssemblyPolicy.isOverLimit(initRes.failed.get, initKept) shouldBe true

    // (c) assets cost: 100 tokens moved, 40,000 of token access above an initial cost of about 12,000
    val tokenTx = ErgoTransaction(IndexedSeq(new Input(tokenBox.id, ProverResult.empty)), IndexedSeq.empty,
      IndexedSeq(new ErgoBoxCandidate(Coin - Fee, TrueTree, next, tokenBox.additionalTokens),
        new ErgoBoxCandidate(Fee, feeProp, next)))
    val (assetsRes, assetsKept) = atLimit(us, tokenTx, 30000)
    assetsRes.failed.get.getMessage should include(": assets cost")
    YoloAssemblyPolicy.isOverLimit(assetsRes.failed.get, assetsKept) shouldBe true

    // (d) cost exceeds limit after input #0: the storage rent path returns a fixed cost without checking the limit
    val oneClaim = StorageRentClaimBuilder.buildClaim(Seq(oldBox), next, params, defaultMinerPk, None, Set.empty).get
    val claimCost = costOf(us, upcoming(us), oneClaim)
    val initial = ErgoInterpreter.interpreterInitCost + params.inputCost + oneClaim.outputs.size * params.outputCost
    val (rentRes, rentKept) = atLimit(us, oneClaim, initial + 1)
    rentRes.failed.get.getMessage should include("cost exceeds limit after input #0")
    rentKept shouldBe None
    YoloAssemblyPolicy.isOverLimit(rentRes.failed.get, rentKept) shouldBe true
    atLimit(us, oneClaim, claimCost)._1 shouldBe Success(claimCost)

    // genuine failures: a script that is false, a script that throws; neither is "over L"
    val (falseRes, falseKept) = atLimit(us, payTx(Seq(youngFalse)), 1000000)
    falseRes.isFailure shouldBe true
    falseKept shouldBe None
    YoloAssemblyPolicy.isOverLimit(falseRes.failed.get, falseKept) shouldBe false
    val ((throwRes, throwKept), throwWarnings) = verifyInputWarnings(atLimit(us, payTx(Seq(throwing)), 1000000))
    throwRes.isFailure shouldBe true
    throwKept shouldBe defined
    throwKept.exists(_.isInstanceOf[CostLimitException]) shouldBe false
    YoloAssemblyPolicy.isOverLimit(throwRes.failed.get, throwKept) shouldBe false
    throwWarnings shouldBe 0

    // the whole loop: the W is a cost liar, the script failures with a record are failures; no WARN line
    val k = key(us)
    val pool = Seq(PoolTx(w, Some(r)), PoolTx(payTx(Seq(youngFalse)), Some(13000)), PoolTx(payTx(Seq(throwing)),
      Some(13000)))
    val (res, loopWarnings) = verifyInputWarnings(collect(us, pool))
    loopWarnings shouldBe 0
    res.outcomeOf(w.id) shouldBe Some(Outcome.CostLiar)
    pool.tail.foreach(p => res.outcomeOf(p.tx.id) shouldBe Some(Outcome.Failed))
    res.refusedBoxIds.toSet shouldBe inputIds(pool.map(_.tx): _*).toSet
    k shouldBe YoloAssemblyPolicy.costKey(us.stateContext.simplifiedUpcoming().currentParameters)
    // the key moves with the parameters validity reads too, not only the cost parameters
    def withParam(id: Byte, v: Int): Parameters =
      Parameters(params.height, params.parametersTable.updated(id, v), params.proposedUpdate)
    YoloAssemblyPolicy.costKey(withParam(Parameters.MinValuePerByteIncrease, params.minValuePerByte + 10)) should not be k
    YoloAssemblyPolicy.costKey(withParam(Parameters.StorageFeeFactorIncrease, params.storageFeeFactor + 25000)) should
      not be k
    YoloAssemblyPolicy.costKey(withParam(Parameters.InputCostIncrease, params.inputCost + 1)) should not be k
  }

  // ========================================================================================= censorship price

  property("the pre-header-branching W (cheap in the mempool's context, 1.34M at assembly) does not " +
    "keep honest txs out, one copy or many; each W is caught at r + slack(r), evicted, its inputs refused, and a " +
    "re-made W from the same boxes is declined at admission") {
    val honest = boxes(0x67000000, 2)
    val us = stateAt(honest, withW = true)
    val ctx = upcoming(us)
    val small = payTx(honest.take(1))
    val small2 = payTx(honest.slice(1, 2))
    val w0 = wTx(0)
    val r = relayCost(us, w0)
    val assembly = costOf(us, ctx, w0)
    info(s"W: relay cost $r, assembly cost $assembly, abort limit ${r + YoloAssemblyPolicy.slack(r)}")
    r should be < 40000
    assembly should be > 1000000

    Seq(1, 6, 30).foreach { copies =>
      withClue(s"$copies copies: ") {
        val ws = (0 until copies).map(wTx(_))
        val pool = admitted(devnet, us, ws ++ Seq(small, small2))
        val memory = new YoloAssemblyMemory
        val (block, eliminate) = candidateWith(us, devnet, pool.getAllPrioritized, memory = memory)
        ids(block) should contain allOf(small.id, small2.id)
        ws.foreach(w => ids(block) should not contain w.id)
        eliminate.ids.toSet shouldBe ws.map(_.id).toSet
        eliminate.refusedBoxIds.toSet shouldBe inputIds(ws: _*).toSet
        info(s"$copies W: candidate of ${block.transactions.size} txs, ${eliminate.ids.size} evicted, " +
          s"${eliminate.refusedBoxIds.size} inputs refused, ${memory.passesForParent} pass(es)")
        applyCandidate(stateAt(honest, withW = true), block)

        // the view holder's handling: the refusal evicts the pool's Ws, and a re-made W from the same boxes is
        // declined without a penalty for the peer (Declined, not Invalidated)
        val refusedPool = eliminate.ids.foldLeft(pool)(_.invalidate(_)).yoloRefuse(eliminate.refusedBoxIds, tip,
          us.boxById)
        refusedPool.size shouldBe 2
        val (_, again) = refusedPool.process(UnconfirmedTransaction(wTx(0, salt = 1), None), us)
        again shouldBe a[ProcessingOutcome.Declined]
        again.asInstanceOf[ProcessingOutcome.Declined].e shouldBe a[YoloRefusedBoxes.Refusal]
      }
    }
  }

  property("sponsor rule: a liar spending a shared box gets it refused, an honest spend of that box alongside " +
    "a signed input of its own is admitted and mined, and a spend of it without a signed input is declined") {
    val shared = box(0x68000000, 0)
    val other = box(0x68000000, 1)
    val userBox = new ErgoBox(Coin, ErgoTree.fromSigmaBoolean(defaultMinerPk), Colls.emptyColl, Map.empty,
      boxId(0x68000000, 2), 0, tip - 10)
    val us = stateAt(Seq(shared, other, userBox), withW = true)
    // the liar: the shared box plus one W box
    val wb = wBoxes.head
    val liar = ErgoTransaction(IndexedSeq(new Input(shared.id, ProverResult.empty), new Input(wb.id, ProverResult.empty)),
      IndexedSeq.empty, IndexedSeq(new ErgoBoxCandidate(shared.value + wb.value - Fee, TrueTree, next),
        new ErgoBoxCandidate(Fee, feeProp, next)))
    val pool = admitted(devnet, us, Seq(liar))
    val (_, eliminate) = candidateWith(us, devnet, pool.getAllPrioritized)
    eliminate.ids shouldBe Seq(liar.id)
    eliminate.refusedBoxIds.toSet shouldBe Set(bytesToId(shared.id), bytesToId(wb.id))
    val refusedPool = pool.invalidate(liar.id).yoloRefuse(eliminate.refusedBoxIds, tip, us.boxById)

    // without a signed input: declined, no penalty
    val bot = payTx(Seq(shared))
    refusedPool.process(UnconfirmedTransaction(bot, None), us)._2 shouldBe a[ProcessingOutcome.Declined]
    // an input of a `true` box with an empty proof is not a sponsor
    val botWithFree = payTx(Seq(shared, other))
    refusedPool.process(UnconfirmedTransaction(botWithFree, None), us)._2 shouldBe a[ProcessingOutcome.Declined]

    // with the user's own signed input: admitted, and mined
    val unsigned = UnsignedErgoTransaction(IndexedSeq(new UnsignedInput(shared.id), new UnsignedInput(userBox.id)),
      IndexedSeq.empty, IndexedSeq(new ErgoBoxCandidate(shared.value + userBox.value - Fee, TrueTree, next),
        new ErgoBoxCandidate(Fee, feeProp, next)))
    val signed = ErgoTransaction(defaultProver.sign(unsigned, IndexedSeq(shared, userBox), IndexedSeq.empty,
      us.stateContext).get)
    signed.inputs(1).spendingProof.proof.nonEmpty shouldBe true
    val (afterSigned, outcome) = refusedPool.process(UnconfirmedTransaction(signed, None), us)
    outcome shouldBe a[ProcessingOutcome.Accepted]
    val (block, eliminated) = candidateWith(us, devnet, afterSigned.getAllPrioritized)
    eliminated.ids shouldBe empty
    ids(block) should contain(signed.id)
    applyCandidate(stateAt(Seq(shared, other, userBox), withW = true), block)
    // when such a tx is caught, the sponsor is refused too: every direct input
    refusedPool.yoloRefuse(inputIds(signed), tip, us.boxById).yoloRefused.refusedInputsOf(signed).toSet shouldBe
      inputIds(signed).toSet
    // switch off: no refused set, admission as upstream
    admitted(switchOff, stateAt(Seq(shared, other, userBox), switchOff, withW = true), Seq(bot))
      .getAllPrioritized.head.costKey shouldBe None
  }

  property("sponsor rule: junk proof bytes on a box that is not a plain P2PK (a third-party `true` box " +
    "or contract box) make no sponsor: a liar beside an already refused box is declined at admission, and a pool tx " +
    "relying on such a sponsor is evicted when its box is refused") {
    val shared1 = box(0x58000000, 0)
    val contract = compileSourceV5("sigmaProp(OUTPUTS(0).value >= SELF.value)", 0)
    val c2 = box(0x58000000, 1, contract)
    val c3 = box(0x58000000, 2, contract)
    val trueBox = box(0x58000000, 3)
    val us = stateAt(Seq(shared1, c2, c3, trueBox), withW = true)
    val wb = wBoxes.head
    val junk = ProverResult(Array.fill(8)(0x5a.toByte), ContextExtension.empty)
    def liar(sponsor: ErgoBox, proof: ProverResult) = ErgoTransaction(
      IndexedSeq(new Input(wb.id, ProverResult.empty), new Input(sponsor.id, proof)), IndexedSeq.empty,
      IndexedSeq(new ErgoBoxCandidate(sponsor.value + wb.value - Fee, TrueTree, next),
        new ErgoBoxCandidate(Fee, feeProp, next)))
    YoloRefusedBoxes.isPlainP2PK(contract) shouldBe false
    YoloRefusedBoxes.isPlainP2PK(TrueTree) shouldBe false
    YoloRefusedBoxes.isPlainP2PK(ErgoTree.fromSigmaBoolean(defaultMinerPk)) shouldBe true

    // round 1: the attacker's one box wb, caught, wb and shared1 refused. A pool tx admitted before the refusal that
    // spends wb with junk bytes on c3 is evicted by the refusal: c3 is no sponsor
    val l1 = liar(shared1, ProverResult.empty)
    val pool1 = admitted(devnet, us, Seq(l1))
    val early = liar(c3, junk)
    val withEarly = ErgoMemPool.empty(devnet).process(UnconfirmedTransaction(early, None), us)._1
    withEarly.contains(early.id) shouldBe true
    val (_, e1) = candidateWith(us, devnet, pool1.getAllPrioritized)
    e1.ids shouldBe Seq(l1.id)
    e1.refusedBoxIds.toSet shouldBe Set(bytesToId(wb.id), bytesToId(shared1.id))
    withEarly.yoloRefuse(e1.refusedBoxIds, tip, us.boxById).contains(early.id) shouldBe false
    val refusedPool = pool1.invalidate(l1.id).yoloRefuse(e1.refusedBoxIds, tip, us.boxById)

    // round 2: wb (refused) beside a third-party box with an empty proof, or with junk proof bytes: declined, so
    // the attacker cannot reuse its refused box and no third-party box gets refused
    Seq(c2 -> ProverResult.empty, c2 -> junk, trueBox -> junk).foreach { case (victim, proof) =>
      val (p, outcome) = refusedPool.process(UnconfirmedTransaction(liar(victim, proof), None), us)
      withClue(s"sponsor ${victim.id.take(4).mkString(",")} with proof of ${proof.proof.length} bytes: ") {
        outcome shouldBe a[ProcessingOutcome.Declined]
        outcome.asInstanceOf[ProcessingOutcome.Declined].e shouldBe a[YoloRefusedBoxes.Refusal]
        p.size shouldBe refusedPool.size
        p.yoloRefused.contains(bytesToId(victim.id)) shouldBe false
      }
    }
  }

  property("Z is kept in tiers by L. Large liars (L = Rg) never crowd a small tx out; 20 liars at L = 49,999 " +
    "crowd a minimal tx out of one pass and 19 do not; through createCandidate the reruns bring it in until 60 liars, " +
    "3 passes per parent; every liar is evicted and its inputs refused") {
    val liarBoxes = boxes(0x69000000, 61 * 25)
    val largeBoxes = boxes(0x6a000000, 5 * 530)
    val tBox = box(0x6b000000, 0)
    val us = stateAt(liarBoxes ++ largeBoxes :+ tBox)
    val k = key(us)
    val t = payTx(Seq(tBox))
    val tRecord = relayCost(us, t)
    val tPool = PoolTx(t, Some(tRecord))
    // tier-1 liars: real cost about 60,000, record 40,909, so L = 40,909 + 4,090 + 5,000 = 49,999
    val liars = (0 until 61).map(i => payTx(liarBoxes.slice(25 * i, 25 * (i + 1))))
    YoloAssemblyPolicy.abortLimit(Some(40909), 2100, Budget, relayLimit) shouldBe 49999
    // large liars: record 990,000, so L = Rg = 1,047,899; real cost above 1,070,000
    val large = (0 until 5).map(i => payTx(largeBoxes.slice(530 * i, 530 * (i + 1))))
    YoloAssemblyPolicy.abortLimit(Some(990000), 2100, Budget, relayLimit) shouldBe
      YoloAssemblyPolicy.guaranteedRoom(Budget, 2100)
    YoloAssemblyPolicy.guaranteedRoom(Budget, 2100) shouldBe 1047899

    Seq(1, 2, 5).foreach { n =>
      val res = collect(us, large.take(n).map(PoolTx(_, Some(990000))) :+ tPool)
      res.txs.map(_.id) should contain(t.id)
      res.outcomeOf(large.head.id) shouldBe Some(Outcome.CostLiar)
      large.slice(1, n).foreach(l => res.outcomeOf(l.id) shouldBe Some(Outcome.NoBudget))
    }
    Seq(19 -> true, 20 -> false).foreach { case (n, in) =>
      withClue(s"$n tier-1 liars, one pass: ") {
        val res = collect(us, liars.take(n).map(PoolTx(_, Some(40909))) :+ tPool)
        res.txs.map(_.id).contains(t.id) shouldBe in
        liars.take(n).foreach(l => res.outcomeOf(l.id) shouldBe Some(Outcome.CostLiar))
        res.refusedBoxIds.toSet shouldBe inputIds(liars.take(n): _*).toSet
        if (!in) res.outcomeOf(t.id) shouldBe Some(Outcome.NoBudget)
        res.zUsed shouldBe n * 49999L
      }
    }
    Seq(59 -> true, 60 -> false).foreach { case (n, in) =>
      withClue(s"$n tier-1 liars, createCandidate: ") {
        val memory = new YoloAssemblyMemory
        val pool = liars.take(n).map(recorded(_, 40909, k)) :+ recorded(t, tRecord, k)
        val (block, eliminate) = candidateWith(us, devnet, pool, memory = memory)
        ids(block).contains(t.id) shouldBe in
        eliminate.ids.toSet shouldBe liars.take(n).map(_.id).toSet
        eliminate.refusedBoxIds.toSet shouldBe inputIds(liars.take(n): _*).toSet
        memory.passesForParent shouldBe 3
        // the next candidate for the same parent, on the stale pool snapshot: the evicted liars are not looked at
        // again, no rerun, and the minimal tx is in
        val (again, againEliminate) = candidateWith(us, devnet, pool, memory = memory)
        ids(again) should contain(t.id)
        againEliminate.ids shouldBe empty
        memory.passesForParent shouldBe 4
      }
    }
  }

  property("R6: a refused box leaves the set when spent in an applied block, or after 2,880 blocks when it is not in " +
    "the UTXO set; entries survive on disk; switch off, the pool never refuses") {
    val boxA = box(0x7e000000, 0)
    val boxB = box(0x7e000000, 1)
    val boxC = box(0x7e000000, 2)
    val ids3 = Seq(boxA, boxB, boxC).map(x => bytesToId(x.id))
    val us = stateAt(Seq(boxA, boxB))
    val pool = ErgoMemPool.empty(devnet).yoloRefuse(ids3, 100, us.boxById)
    pool.yoloRefused.size shouldBe 3
    val inUtxo: ModifierId => Boolean = id => us.boxById(scorex.crypto.authds.ADKey @@ scorex.util.idToBytes(id)).isDefined
    // a spent in a block; c (not in the UTXO set) kept until 2,880 blocks after it was refused; b kept
    val early = pool.yoloReleaseRefused(Seq(ids3.head), 100 + YoloRefusedBoxes.AgeLimit - 1, inUtxo)
    early.yoloRefused.entries.keySet shouldBe ids3.tail.toSet
    val late = early.yoloReleaseRefused(Seq.empty, 100 + YoloRefusedBoxes.AgeLimit, inUtxo)
    late.yoloRefused.entries.keySet shouldBe Set(ids3(1))
    // a box in the UTXO set is kept however old the entry
    late.yoloReleaseRefused(Seq.empty, 100 + 100 * YoloRefusedBoxes.AgeLimit, inUtxo).yoloRefused.size shouldBe 1
    // an old entry whose box is in the UTXO set is re-stamped at the height it was checked at, so it is not
    // looked up again until AgeLimit blocks later
    var lookups = 0
    val counting: ModifierId => Boolean = { id => lookups += 1; inUtxo(id) }
    val checkedAt = 100 + 5 * YoloRefusedBoxes.AgeLimit
    val restamped = late.yoloReleaseRefused(Seq.empty, checkedAt, counting)
    lookups shouldBe 1
    restamped.yoloRefused.entries shouldBe Map(ids3(1) -> checkedAt)
    (1 until YoloRefusedBoxes.AgeLimit).foldLeft(restamped) { case (p, d) =>
      p.yoloReleaseRefused(Seq.empty, checkedAt + d, counting) }.yoloRefused.entries shouldBe Map(ids3(1) -> checkedAt)
    lookups shouldBe 1
    restamped.yoloReleaseRefused(Seq.empty, checkedAt + YoloRefusedBoxes.AgeLimit, counting).yoloRefused.entries shouldBe
      Map(ids3(1) -> (checkedAt + YoloRefusedBoxes.AgeLimit))
    lookups shouldBe 2

    // the disk store: written by difference, read back at start
    val store = YoloRefusedBoxesStore.open(createTempDir.getAbsolutePath).get
    store.update(YoloRefusedBoxes.empty, pool.yoloRefused)
    store.load().entries shouldBe pool.yoloRefused.entries
    store.update(pool.yoloRefused, late.yoloRefused)
    store.load().entries shouldBe late.yoloRefused.entries
    store.close()

    // switch off: admission ignores the set (the node never fills it with the switch off)
    val usOff = stateAt(Seq(boxA, boxB), switchOff)
    val offPool = ErgoMemPool.empty(switchOff).withYoloRefused(pool.yoloRefused)
    offPool.process(UnconfirmedTransaction(payTx(Seq(boxA)), None), usOff)._2 shouldBe a[ProcessingOutcome.Accepted]
  }

  // ========================================================================================= no stop by txs that do not fit

  property("1,000 txs whose L is above the room, behind an honest large tx, are passed over at zero work (their " +
    "inputs are not even looked up), are not reported invalid or penalized, and the small tx behind them is in; an " +
    "honest tx that does not fit stays in the pool unpenalized") {
    val bigBoxes = boxes(0x6c000000, 800)
    val smallBox = box(0x6d000000, 0)
    val us = stateAt(bigBoxes :+ smallBox)
    val ctx = upcoming(us)
    val k = key(us)
    val big = txAtCost(us, ctx, bigBoxes.take(320), 600000)
    val big2 = txAtCost(us, ctx, bigBoxes.slice(320, 800), 750000)
    val small = payTx(Seq(smallBox))
    // 1,000 txs spending boxes that are in no state: any lookup would report them invalid (missing inputs)
    val never = (0 until 1000).map(i => payTx(Seq(box(0x6e000000, i))))
    val pool = Seq(recorded(big, relayCost(us, big), k), recorded(big2, relayCost(us, big2), k)) ++
      never.map(recorded(_, 900000, k)) :+ recorded(small, relayCost(us, small), k)

    val res = collect(us, pool.map(u => PoolTx(u.transaction, u.lastCost)))
    res.invalid shouldBe empty
    res.zUsed shouldBe 0L
    res.txs.map(_.id) should contain allOf(big.id, small.id)
    res.outcomeOf(big2.id) shouldBe Some(Outcome.NoRoom)
    never.foreach(n => res.outcomeOf(n.id) shouldBe Some(Outcome.NoRoom))

    val (block, eliminate) = candidateWith(us, devnet, pool)
    ids(block) should contain allOf(big.id, small.id)
    ids(block) should not contain big2.id
    eliminate.ids shouldBe empty
    eliminate.refusedBoxIds shouldBe empty
  }

  property("a tx at the relay limit (its cost plus what its fee output adds to the fee tx) is included " +
    "behind a rent claim at its 270,000 budget; the block applies") {
    val atLimitBoxes = boxes(0x6f000000, 600)
    val us = stateAt(atLimitBoxes)
    val ctx = upcoming(us)
    val atLimit = txAtCost(us, ctx, atLimitBoxes, relayLimit - 2100)
    val pool = admitted(devnet, us, Seq(atLimit))
    val (block, eliminate) = candidateWith(us, devnet, pool.getAllPrioritized, rent = tokenRentBoxes)
    eliminate.ids shouldBe empty
    val claim = block.transactions(1)
    claim.inputs.exists(i => tokenRentBoxes.exists(b => java.util.Arrays.equals(b.id, i.boxId))) shouldBe true
    val claimCost = costOf(us, ctx, claim)
    claimCost should be <= CandidateGenerator.YoloRentClaimCostBudget
    claimCost should be > 250000
    ids(block)(2) shouldBe atLimit.id
    info(s"claim of ${claim.inputs.size} token boxes costing $claimCost; tx at the relay limit costing " +
      s"${costOf(us, ctx, atLimit)}")
    applyCandidate(stateAt(atLimitBoxes), block)
  }

  property("a paid X (160 dust fee outputs) and six waiters (441 each) do not keep an honest tx " +
    "out, with and without a rent claim; nothing is evicted") {
    val inputs = boxes(0x70000000, 8)
    val us = stateAt(inputs)
    val params = us.stateContext.currentParameters
    val dust = new ErgoBoxCandidate(1000000L, feeProp, next).toBox(bytesToId(Array.fill(32)(0: Byte)), 0).bytes.length.toLong *
      params.minValuePerByte + 1000
    def dustTx(input: ErgoBox, n: Int): ErgoTransaction = ErgoTransaction(IndexedSeq(new Input(input.id, ProverResult.empty)),
      IndexedSeq.empty, (new ErgoBoxCandidate(input.value - n * dust, TrueTree, next) +:
        Seq.fill(n)(new ErgoBoxCandidate(dust, feeProp, next))).toIndexedSeq)
    val x = dustTx(inputs(0), 160)
    val waiters = (1 to 6).map(i => dustTx(inputs(i), 441))
    val small = payTx(Seq(inputs(7)))
    val pool = admitted(devnet, us, (x +: waiters) :+ small)
    Seq(rentBoxes, Seq.empty[ErgoBox]).foreach { rent =>
      val (block, eliminate) = candidateWith(us, devnet, pool.getAllPrioritized, rent = rent)
      ids(block) should contain(small.id)
      eliminate.ids shouldBe empty
      eliminate.refusedBoxIds shouldBe empty
      // a waiter that fits (ranked first) is mined and pays its fee; two never fit together
      waiters.count(w => ids(block).contains(w.id)) should be <= 1
    }
  }

  property("size: 96 KB txs that do not fit by size are passed over at zero work and a small tx behind them " +
    "is in") {
    val bigInputs = boxes(0x71000000, 7)
    val smallBox = box(0x71000000, 100)
    val us = stateAt(bigInputs :+ smallBox)
    def heavy(input: ErgoBox): ErgoTransaction = payTx(Seq(input), outs = (0 until 24).map(j =>
      new ErgoBoxCandidate(2000000L, TrueTree, next, Colls.emptyColl,
        Map(ErgoBox.R4 -> ByteArrayConstant(Array.fill(3900)(j.toByte))))))
    val heavies = bigInputs.map(heavy)
    heavies.head.size should be > 94000
    val small = payTx(Seq(smallBox))
    val pool = heavies.map(h => PoolTx(h, Some(relayCost(us, h)))) :+ PoolTx(small, Some(relayCost(us, small)))
    val res = collect(us, pool)
    val in = heavies.count(h => res.txs.exists(_.id == h.id))
    info(s"96 KB txs in: $in of ${heavies.size}; ${res.txs.map(_.size).sum} bytes in the candidate")
    in should be >= 4
    in should be < heavies.size
    heavies.filterNot(h => res.txs.exists(_.id == h.id)).foreach(h => res.outcomeOf(h.id) shouldBe Some(Outcome.NoSize))
    res.txs.map(_.id) should contain(small.id)
    res.invalid shouldBe empty
    res.zUsed shouldBe 0L
    res.txs.map(_.size).sum should be < us.stateContext.currentParameters.maxBlockSize
  }

  property("no-record path: a tx with no record, or with a record under another key, is never penalized: over " +
    "its limit or invalid it is evicted without refusing its inputs; valid above the room it is kept, charged, and " +
    "its cost is recorded for the next candidate") {
    val young = box(0x72000000, 0, FalseTree)
    val bigBoxes = boxes(0x73000000, 320)
    val us = stateAt(bigBoxes :+ young, withW = true)
    val ctx = upcoming(us)
    val k = key(us)
    val w = wTx(0)
    val falseTx = payTx(Seq(young))
    // no record: W costs 1.34M at assembly, above the relay limit in use, and a script that is false: policy
    // breaks, no refusal (each without a record draws on the top Z tier with L = the relay limit, so one per pass)
    Seq(w, falseTx).foreach { tx =>
      val res = collect(us, Seq(PoolTx(tx, None)))
      res.outcomeOf(tx.id) shouldBe Some(Outcome.PolicyBreak)
      res.invalid shouldBe Seq(tx.id)
      res.refusedBoxIds shouldBe empty
    }
    // a record under another key (a vote changed a cost parameter): the same, through createCandidate (the second
    // one is examined in the rerun)
    val (_, eliminate) = candidateWith(us, devnet, Seq(recorded(w, relayCost(us, w), k + 1),
      recorded(falseTx, 13000, k + 1)))
    eliminate.ids.toSet shouldBe Set(w.id, falseTx.id)
    eliminate.refusedBoxIds shouldBe empty

    // valid, above the room, within its limit: kept and charged; the generator records its cost. Five W boxes
    // without the cheap branch's context cost about 745,000, valid, with a no-script lower bound of 20,200
    val big = txAtCost(us, ctx, bigBoxes, 600000)
    val heavyW = ErgoTransaction(wBoxes.slice(9, 14).map(b => new Input(b.id, ProverResult.empty)), IndexedSeq.empty,
      IndexedSeq(new ErgoBoxCandidate(5 * Coin - Fee, TrueTree, next), new ErgoBoxCandidate(Fee, feeProp, next)))
    val heavyCost = costOf(us, ctx, heavyW)
    info(s"five W boxes at assembly: $heavyCost")
    heavyCost should (be > 600000 and be < 1000000)
    val memory = new YoloAssemblyMemory
    val (block, kept) = candidateWith(us, devnet, Seq(recorded(big, relayCost(us, big), k),
      UnconfirmedTransaction(heavyW, None)), memory = memory)
    kept.ids shouldBe empty
    kept.refusedBoxIds shouldBe empty
    ids(block) should contain(big.id)
    ids(block) should not contain heavyW.id
    memory.record(heavyW.id, k) shouldBe Some(heavyCost)
    val again = collect(us, Seq(PoolTx(big, Some(relayCost(us, big))), PoolTx(heavyW, memory.record(heavyW.id, k))))
    again.outcomeOf(heavyW.id) shouldBe Some(Outcome.NoRoom)
    again.zUsed shouldBe 0L
  }

  // ========================================================================================= work bound

  property("a pool of 1,000 hostile txs (cost liars with records from 12,201 to 990,000 in all three tiers, " +
    "script failures, no-record failures): work on txs left out stays within each Z tier and within L each; no tx " +
    "past the budget is looked up; at most 3 passes per parent end short of Z") {
    // tiers: L up to 50,000, up to 250,000, above; a liar's real cost is above its L through its inputs
    def recordFor(i: Int): Int = (i % 4) match {
      case 0 => 12201 + (i * 37) % (40909 - 12201)
      case 1 => 40910 + (i * 101) % (222726 - 40910)
      case 2 => 222730 + (i * 997) % (990000 - 222730)
      case _ => 13000
    }
    def limitOf(r: Int): Long = YoloAssemblyPolicy.abortLimit(Some(r), 2100, Budget, relayLimit)
    // how many of each kind can be reached in 3 passes: Z tier / smallest L of the tier, times 3
    val reachable = Map(0 -> 3 * (1000000 / 18421 + 1), 1 -> 3 * (1000000 / 50001 + 1), 2 -> 3 * (1050000 / 250001 + 1),
      3 -> 3 * (1000000 / 18421 + 1), 4 -> 3 * 2)
    val counts = mutable.Map.empty[Int, Int].withDefaultValue(0)
    var prefix = 0x74000000
    val realBoxes = mutable.ArrayBuffer.empty[ErgoBox]
    val pool: Seq[(ErgoTransaction, Option[Int])] = (0 until 1000).map { i =>
      val kind = if (i % 10 == 9) 4 else i % 4
      val n = counts(kind)
      counts(kind) = n + 1
      val real = n < reachable(kind)
      prefix += 1
      kind match {
        case 4 | 3 =>
          // script false (a young `false` box), with a record (failure) or without (policy break)
          val b = box(prefix, 0, FalseTree)
          if (real) realBoxes += b
          payTx(Seq(b)) -> (if (kind == 3) Some(recordFor(i)) else None)
        case _ =>
          val r = recordFor(i)
          val inputs = (limitOf(r) / 2000 + 2).toInt
          val bs = boxes(prefix, inputs)
          if (real) realBoxes ++= bs
          payTx(bs) -> Some(r)
      }
    }
    val us = stateAt(realBoxes.toSeq)
    val k = key(us)

    val res = collect(us, pool.map { case (tx, r) => PoolTx(tx, r) })
    res.outcomes.foreach { case (_, outcome, charge, limit) =>
      charge should be <= math.max(0L, limit)
      outcome should not be Outcome.Missing
    }
    res.zUsed should be <= YoloAssemblyPolicy.ZTiers.map(_._2).sum
    YoloAssemblyPolicy.ZTiers.indices.foreach { tier =>
      val (upTo, budget) = YoloAssemblyPolicy.ZTiers(tier)
      val lower = if (tier == 0) 0L else YoloAssemblyPolicy.ZTiers(tier - 1)._1
      res.outcomes.filter { case (_, _, _, l) => l > lower && l <= upTo }.map(_._3).sum should be <= budget
    }
    val charged = res.outcomes.count(_._3 > 0)
    info(s"one pass: ${res.outcomes.groupBy(_._2).map { case (o, s) => s"$o ${s.size}" }.mkString(", ")}; " +
      s"Z used ${res.zUsed}, $charged txs charged")
    res.passedOverForZ shouldBe true

    val memory = new YoloAssemblyMemory
    val utxs = pool.map { case (tx, r) => r.fold(UnconfirmedTransaction(tx, None))(recorded(tx, _, k)) }
    val (_, eliminate) = candidateWith(us, devnet, utxs, memory = memory)
    memory.passesForParent shouldBe 3
    memory.zPassesForParent shouldBe 3
    eliminate.ids.size should be <= reachable.values.sum
    // refused: the inputs of the evicted txs with a record, never those of the no-record ones
    val noRecord = pool.collect { case (tx, None) => tx }
    eliminate.refusedBoxIds.toSet.intersect(inputIds(noRecord: _*).toSet) shouldBe empty
    // the next candidate for this parent runs one pass, without a rerun
    candidateWith(us, devnet, utxs, memory = memory)
    memory.passesForParent shouldBe 4
  }

  // ========================================================================================= room for the mandatory txs

  property("a claim of register-heavy boxes is kept within its 64 KB size budget and a 96 KB pool tx is still " +
    "in; a claim of old fee boxes counts what the fee tx adds for them, so emission, claim and fee tx stay within " +
    "the reserve and a tx at the relay limit still fits; both blocks apply") {
    val heavyRent = (0 until 100).map(i => box(0x75000000, i, FalseTree, BoxValue,
      regs = Map(ErgoBox.R4 -> ByteArrayConstant(Array.fill(3900)(i.toByte))), height = next - StoragePeriod - 10))
    val feeRent = (0 until 100).map(i => box(0x76000000, i, feeProp, BoxValue, height = next - StoragePeriod - 10))
    val heavyInput = box(0x77000000, 0)
    val atLimitBoxes = boxes(0x78000000, 600)
    val extra = heavyRent ++ feeRent ++ atLimitBoxes :+ heavyInput
    val us = stateAt(extra)
    val ctx = upcoming(us)
    val params = ctx.currentParameters
    val heavy = payTx(Seq(heavyInput), outs = (0 until 24).map(j => new ErgoBoxCandidate(2000000L, TrueTree, next,
      Colls.emptyColl, Map(ErgoBox.R4 -> ByteArrayConstant(Array.fill(3900)(j.toByte))))))
    heavy.size should be > 94000

    // register-heavy: 100 boxes of about 4 KB; the cost budget alone would take all 100 (about 400 KB)
    val pool = admitted(devnet, us, Seq(heavy))
    val (block, eliminate) = candidateWith(us, devnet, pool.getAllPrioritized, rent = heavyRent)
    eliminate.ids shouldBe empty
    val claim = block.transactions(1)
    claim.inputs.exists(i => heavyRent.exists(b => java.util.Arrays.equals(b.id, i.boxId))) shouldBe true
    claim.size should be <= YoloAssemblyPolicy.RentClaimSizeBudget
    claim.inputs.size should be < 100
    ids(block) should contain(heavy.id)
    info(s"register-heavy claim: ${claim.inputs.size} boxes, ${claim.size} bytes; 96 KB tx in")
    applyCandidate(stateAt(extra), block)

    // old fee boxes: each recreation is a fee box the fee tx collects, at up to 2,100 more
    val atLimit = txAtCost(us, ctx, atLimitBoxes, relayLimit - 2100)
    val (feeBlock, feeEliminate) = candidateWith(us, devnet, admitted(devnet, us, Seq(atLimit)).getAllPrioritized,
      rent = feeRent)
    feeEliminate.ids shouldBe empty
    val feeClaim = feeBlock.transactions(1)
    val recreatedFeeBoxes = feeClaim.outputs.count(o => YoloFeeOutputPolicy.isFeeOutput(o, devnet.chainSettings))
    recreatedFeeBoxes should be > 0
    val feeTx = feeBlock.transactions.last
    val feeTxCost = feeTx.statefulValidity(feeTx.inputs.map(i => feeBlock.transactions.flatMap(_.outputs)
      .find(b => java.util.Arrays.equals(b.id, i.boxId)).get), IndexedSeq.empty, ctx)(ErgoInterpreter(params)).get
    val mandatory = costOf(us, ctx, emissionTx) + costOf(us, ctx, feeClaim) + feeTxCost
    info(s"claim of ${feeClaim.inputs.size} old fee boxes ($recreatedFeeBoxes recreated at the fee proposition): " +
      s"emission, claim and fee tx cost $mandatory")
    mandatory should be <= CandidateGenerator.YoloMandatoryTxsCostReserve
    ids(feeBlock) should contain(atLimit.id)
    applyCandidate(stateAt(extra), feeBlock)
    // without the fee boxes in the bound, the cost budget alone takes all 100 boxes
    StorageRentClaimBuilder.buildClaim(feeRent, next, params, defaultMinerPk, None, Set.empty,
      maxCost = Some(CandidateGenerator.YoloRentClaimCostBudget.toLong)).get.inputs.size shouldBe 100
  }

  property("the relay limit in use follows a vote that lowers the block cost limit, at admission; the " +
    "abort limit never exceeds Rg") {
    YoloAssemblyPolicy.relayLimitInUse(relayLimit, YoloLaunchParameters) shouldBe relayLimit
    val lowered = YoloLaunchParameters.withBlockCost(1300000)
    YoloAssemblyPolicy.relayLimitInUse(relayLimit, lowered) shouldBe 1300000 - 150000 - 300000 - 2
    val bs = boxes(0x79000000, 1000)
    val us = stateAt(bs)
    val usLow = stateAt(bs, params = lowered)
    // own cost just under 849,000: with its fee output, over 849,998
    val tx = txAtCost(us, upcoming(us), bs.take(500), 849000)
    admitted(devnet, us, Seq(tx)).size shouldBe 1
    val (_, low) = ErgoMemPool.empty(devnet).process(UnconfirmedTransaction(tx, None), usLow)
    low shouldBe a[ProcessingOutcome.Declined]
    low.asInstanceOf[ProcessingOutcome.Declined].e.getMessage should include("transaction cost limit 849998")
    // own cost above 849,998: over the limit itself, refused as any tx over the cost limit
    val tx2 = txAtCost(us, upcoming(us), bs.drop(500), 900000)
    admitted(devnet, us, Seq(tx2)).size shouldBe 1
    ErgoMemPool.empty(devnet).process(UnconfirmedTransaction(tx2, None), usLow)._2 shouldBe
      a[ProcessingOutcome.Invalidated]
    // switch off: as upstream, the configured limit
    admitted(switchOff, stateAt(bs, switchOff, lowered), Seq(tx, tx2)).size shouldBe 2

    Seq(12201, 40909, 500000, 950000, 990000, 1000000).foreach { r =>
      Seq(0L, 2100L, 50000L).foreach { f =>
        YoloAssemblyPolicy.abortLimit(Some(r), f, Budget, relayLimit) should be <= YoloAssemblyPolicy.guaranteedRoom(Budget, f)
      }
    }
  }

  property("after a vote lowers the block cost limit to 1,052,094, a tx at the old boundary " +
    "(own cost plus its fee output's addition equal to B - M) is declined at admission, and a tx with a record above " +
    "Rg is evicted (no refusal), never kept and passed over forever") {
    val bs = boxes(0x59000000, 400)
    val us0 = stateAt(bs)
    val tx = txAtCost(us0, upcoming(us0), bs, 600000)
    val c = relayCost(us0, tx)
    val f = YoloFeeOutputPolicy.feeCollectionCost(tx, devnet.chainSettings, YoloLaunchParameters)
    val x = (c + f + 450000).toInt
    info(s"cost $c, f $f, block cost limit voted to $x")
    val lowered = YoloLaunchParameters.withBlockCost(x)
    val us = stateAt(bs, params = lowered)
    val lowParams = upcoming(us).currentParameters
    val relayInUse = YoloAssemblyPolicy.relayLimitInUse(relayLimit, lowParams)
    val rg = YoloAssemblyPolicy.guaranteedRoom(YoloAssemblyPolicy.budget(lowParams), f)
    relayInUse shouldBe c + f - 2
    // no admissible record is above Rg - 1: record + f <= relay limit in use = Rg - 1 + f
    (relayInUse - f) shouldBe rg - 1
    // the boundary tx is declined at admission
    val (_, admission) = ErgoMemPool.empty(devnet).process(UnconfirmedTransaction(tx, None), us)
    admission shouldBe a[ProcessingOutcome.Declined]
    // a record above Rg (as after a down-vote) is an eviction without refusal, in one pass and in createCandidate
    val res = collect(us, Seq(PoolTx(tx, Some(c))))
    res.outcomeOf(tx.id) shouldBe Some(Outcome.Unmeasurable)
    res.invalid shouldBe Seq(tx.id)
    res.refusedBoxIds shouldBe empty
    res.zUsed shouldBe 0L
    val (block, e) = candidateWith(us, devnet, Seq(recorded(tx, c, key(us))))
    ids(block) should not contain tx.id
    e.ids shouldBe Seq(tx.id)
    e.refusedBoxIds shouldBe empty
  }

  property("where a voted block cost limit makes the relay limit in use bind, a tx whose cost plus " +
    "its fee output's addition is exactly that limit is accepted, included by createCandidate, not evicted, no input " +
    "refused, and the block applies; one unit above the limit it is declined at admission") {
    val bs = boxes(0x5a000000, 400)
    val us0 = stateAt(bs)
    val tx = txAtCost(us0, upcoming(us0), bs, 600000)
    val c = relayCost(us0, tx)
    val f = YoloFeeOutputPolicy.feeCollectionCost(tx, devnet.chainSettings, YoloLaunchParameters)
    // B - M - 2 = c + f: x - 150,000 - 300,000 - 2
    val x = (c + f + 450002).toInt
    info(s"cost $c, f $f, block cost limit voted to $x")
    val lowered = YoloLaunchParameters.withBlockCost(x)
    val us = stateAt(bs, params = lowered)
    val lowParams = upcoming(us).currentParameters
    val relayInUse = YoloAssemblyPolicy.relayLimitInUse(relayLimit, lowParams)
    relayInUse shouldBe c + f
    relayInUse should be < relayLimit
    // its abort limit is c + 1, within Rg
    YoloAssemblyPolicy.abortLimit(Some(c), f, YoloAssemblyPolicy.budget(lowParams), relayInUse) shouldBe c + 1L
    YoloAssemblyPolicy.guaranteedRoom(YoloAssemblyPolicy.budget(lowParams), f) shouldBe c + 1L

    // exactly at the limit: accepted with record c, included, nothing evicted or refused, the block applies
    val pool = admitted(devnet, us, Seq(tx))
    pool.getAll.head.recordedCost(key(us)) shouldBe Some(c)
    val (block, e) = candidateWith(us, devnet, pool.getAllPrioritized)
    ids(block) should contain(tx.id)
    e.ids shouldBe empty
    e.refusedBoxIds shouldBe empty
    applyCandidate(stateAt(bs, params = lowered), block)

    // one unit above the limit in use (the block cost limit one lower): declined at admission
    val usAbove = stateAt(bs, params = YoloLaunchParameters.withBlockCost(x - 1))
    YoloAssemblyPolicy.relayLimitInUse(relayLimit, upcoming(usAbove).currentParameters) shouldBe c + f - 1
    ErgoMemPool.empty(devnet).process(UnconfirmedTransaction(tx, None), usAbove)._2 shouldBe
      a[ProcessingOutcome.Declined]
  }

  // ========================================================================================= switch off

  property("switch off: createCandidate gives exactly what upstream v6.0.8's collectTxs gives (reference copy) " +
    "for the hostile pools above, and never refuses an input") {
    val honest = boxes(0x7a000000, 2)
    val liarBoxes = boxes(0x7b000000, 3 * 25)
    val largeBoxes = boxes(0x7c000000, 2 * 530)
    val extra = honest ++ liarBoxes ++ largeBoxes
    val usOff = stateAt(extra, switchOff, withW = true)
    val ctxOff = upcoming(usOff)
    ctxOff.chainSettings.yolo.enabled shouldBe false
    val small = payTx(honest.take(1))
    val small2 = payTx(honest.slice(1, 2))
    val liars = (0 until 3).map(i => payTx(liarBoxes.slice(25 * i, 25 * (i + 1))))
    val large = (0 until 2).map(i => payTx(largeBoxes.slice(530 * i, 530 * (i + 1))))
    val never = (0 until 100).map(i => payTx(Seq(box(0x7d000000, i))))
    val pools: Seq[(String, Seq[ErgoTransaction])] = Seq(
      "W x6, small, small2" -> ((0 until 6).map(wTx(_)) ++ Seq(small, small2)),
      "liars and large liars, small" -> (liars ++ large :+ small),
      "100 txs missing their inputs, small" -> (never :+ small))
    val claim = StorageRentClaimBuilder.buildClaim(rentBoxes, next, YoloLaunchParameters, defaultMinerPk, None,
      Set.empty).get
    pools.foreach { case (name, txs) =>
      withClue(s"$name: ") {
        val utxs = txs.map(UnconfirmedTransaction(_, None))
        val (block, eliminate) = candidateWith(usOff, switchOff, utxs)
        val (upTxs, upInvalid) = UpstreamV608Assembly.collectTxs(defaultMinerPk, Budget,
          ctxOff.currentParameters.maxBlockSize, usOff, ctxOff, Seq(emissionTx, claim) ++ txs)
        ids(block) shouldBe upTxs.map(_.id)
        eliminate.ids shouldBe upInvalid
        eliminate.refusedBoxIds shouldBe empty
        info(s"$name: ${block.transactions.size} txs, ${eliminate.ids.size} eliminated, as upstream")
      }
    }
  }

}
