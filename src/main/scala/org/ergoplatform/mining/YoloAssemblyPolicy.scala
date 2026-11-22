package org.ergoplatform.mining

import org.ergoplatform.ErgoTreePredef
import org.ergoplatform.mining.CandidateGenerator.{CostedTransaction, YoloMandatoryTxsCostReserve, correctLimits, doublespend}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.mempool.YoloFeeOutputPolicy
import org.ergoplatform.nodeView.state.{UpcomingStateContext, UtxoStateReader}
import org.ergoplatform.settings.Parameters
import org.ergoplatform.validation.{ModifierError, MultipleErrors}
import org.ergoplatform.wallet.interpreter.ErgoInterpreter
import org.ergoplatform.sdk.BlockchainParameters
import scorex.util.{ModifierId, ScorexLogging, bytesToId}
import sigma.ast.ErgoTree
import sigma.data.ProveDlog
import sigma.exceptions.CostLimitException
import sigmastate.interpreter.Interpreter.{ScriptEnv, VerificationResult}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.hashing.MurmurHash3
import scala.util.{Failure, Success, Try}

/**
  * YOLO block assembly policy (switch on only, `ergo.chain.yolo.enabled`): node policy for which pool transactions a
  * candidate carries. Nothing here changes which blocks are valid. With the switch off `CandidateGenerator.collectTxs`
  * runs exactly as upstream v6.0.8 and nothing here is used.
  *
  * A pool transaction is held to the cost this node recorded for it (its record r: the highest cost measured for it
  * under the current cost parameters and block version, `costKey`). At assembly it is validated with an abort limit
  * L = min(r + slack(r), Rg), where Rg is the room every pool transaction is guaranteed when it is the first one. A
  * transaction that goes over L is a cost liar: it is evicted and its direct inputs are refused at admission until
  * they are spent on chain (`YoloRefusedBoxes`, sponsor rule). A transaction whose L does not fit the room left is
  * passed over at zero work, so a valid transaction that does not fit never ends assembly. Work spent on transactions
  * that end up left out is charged to a per-candidate budget Z, in three tiers by L, so that small transactions are
  * not crowded out by a few large liars.
  */
object YoloAssemblyPolicy extends ScorexLogging {

  /** Slack over the record: r / 10 + 5,000 */
  def slack(r: Int): Int = r / 10 + 5000

  /**
    * Work budget Z per candidate for transactions that are left out, in three tiers by abort limit L:
    * L up to 50,000 draws on 1,000,000, L up to 250,000 on 1,000,000, larger L on 1,050,000.
    */
  val ZTiers: IndexedSeq[(Long, Long)] = IndexedSeq(50000L -> 1000000L, 250000L -> 1000000L, Long.MaxValue -> 1050000L)

  /** Most assembly passes per parent that end with a transaction passed over for Z and one evicted */
  val MaxZPassesPerParent: Int = 3

  /** Size budget of the miner's own rent claim, in bytes */
  val RentClaimSizeBudget: Int = 64 * 1024

  /**
    * Record key: a hash of the cost parameters, the parameters validity reads (minimum value per byte, storage fee
    * factor) and the block version, from `params`. A record measured under another key is not used (no penalty can
    * follow from it). The miner key is not part of it.
    */
  def costKey(params: Parameters): Int = MurmurHash3.orderedHash(Seq(params.blockVersion.toInt, params.inputCost,
    params.dataInputCost, params.outputCost, params.tokenAccessCost, params.minValuePerByte, params.storageFeeFactor))

  /** Assembly budget B: the block cost limit less the safe gap */
  def budget(params: Parameters): Int = params.maxBlockCost - CandidateGenerator.safeGap(params.maxBlockCost)

  /**
    * Relay limit in use: `maxTransactionCost`, but never above B - M - 2 for `params`, so that every
    * admitted transaction has a record of at most Rg - 1 (`guaranteedRoom`) and so an abort limit L of at least r + 1:
    * validation at a limit equal to the measured cost fails (the interpreter checks in JitCost, the measured cost is
    * rounded down to block cost units), so a transaction costing c needs a limit of c + 1
    */
  def relayLimitInUse(maxTransactionCost: Int, params: Parameters): Int =
    math.min(maxTransactionCost, budget(params) - YoloMandatoryTxsCostReserve - 2)

  /** Rg: room a pool transaction whose fee outputs add `f` to the fee transaction is guaranteed as the first one */
  def guaranteedRoom(b: Int, f: Long): Long = b - 1L - YoloMandatoryTxsCostReserve - f

  /**
    * Abort limit L. With a record r: min(r + slack(r), Rg). Without: the relay limit in use (the cost relay admission
    * allows; a transaction with no record never draws a penalty).
    */
  def abortLimit(record: Option[Int], f: Long, b: Int, relayInUse: Int): Long = record match {
    case Some(r) => math.min(r.toLong + slack(r), guaranteedRoom(b, f))
    case None => relayInUse.toLong
  }

  /** Lower bound of a transaction's validation cost that runs no script: init cost, inputs, data inputs, outputs */
  def noScriptLowerBound(tx: ErgoTransaction, params: Parameters): Long =
    ErgoInterpreter.interpreterInitCost.toLong + tx.inputs.size.toLong * params.inputCost +
      tx.dataInputs.size.toLong * params.dataInputCost + tx.outputCandidates.size.toLong * params.outputCost

  /**
    * Upper bound of the fee transaction's size: a fixed part (counts, value, height, the miner script of
    * `minerPropSize` bytes), 34 bytes per fee box spent (id, empty proof, empty extension), 44 per token entry of the
    * fee boxes (id in the token table, index and amount in the output). 0 without fee boxes (no fee transaction).
    */
  def feeTxSizeBound(minerPropSize: Int, feeBoxes: Int, tokenEntries: Int): Long =
    if (feeBoxes == 0) 0L else 64L + minerPropSize + 34L * feeBoxes + 44L * tokenEntries

  /** Upper bound of the fee transaction's cost: base plus f per fee box; 0 without fee boxes */
  def feeTxCostBound(params: Parameters, feeBoxes: Int, feeCollection: Long): Long =
    if (feeBoxes == 0) 0L else YoloFeeOutputPolicy.feeTxBaseCost(params) + feeCollection

  /**
    * Verifier for the assembly path only: a failure of a script (an exception, a cost limit hit among them) is turned
    * into a `false` result, and the first one is kept for the caller. `ErgoTransaction.verifyInput` then writes
    * neither of its WARN lines (the second dumps the whole context). Block validation keeps its own verifier.
    */
  final class YoloAssemblyVerifier(params: BlockchainParameters) extends ErgoInterpreter(params) {
    private var kept: Option[Throwable] = None

    /** Forget the failure kept for the previous transaction */
    def reset(): Unit = kept = None

    /** The first script failure since the last `reset` */
    def failure: Option[Throwable] = kept

    override def verify(env: ScriptEnv,
                        exp: ErgoTree,
                        context: CTX,
                        proof: Array[Byte],
                        message: Array[Byte]): Try[VerificationResult] =
      super.verify(env, exp, context, proof, message) match {
        case Failure(t) =>
          if (kept.isEmpty) kept = Some(t)
          Success((false, 0L))
        case s => s
      }
  }

  private val CostRuleMessage = "Accumulated cost of block transactions should not exceed"

  @tailrec
  private def hasCostLimitCause(t: Throwable, depth: Int = 0): Boolean = t match {
    case null => false
    case _: CostLimitException => true
    case _ if depth > 10 => false
    case _ => hasCostLimitCause(t.getCause, depth + 1)
  }

  private def isCostRuleFailure(e: ModifierError): Boolean = {
    val m = e.message
    m.contains(CostRuleMessage) &&
      (m.contains(": initial cost") || m.contains(": assets cost") || m.contains("cost exceeds limit after input #"))
  }

  /**
    * Whether a validation failure at abort limit L means "over L": a `CostLimitException` kept by the
    * verifier, or a `bsBlockTransactionsCost` failure on "initial cost", "assets cost" or "cost exceeds limit after
    * input #i" (the last one is reached through the storage rent path, which returns a fixed cost without checking the
    * limit). A genuine script failure shows none of them.
    */
  def isOverLimit(failure: Throwable, kept: Option[Throwable]): Boolean = {
    kept.exists(hasCostLimitCause(_)) || hasCostLimitCause(failure) || (failure match {
      case e: ModifierError => isCostRuleFailure(e)
      case MultipleErrors(errors) => errors.exists(isCostRuleFailure)
      case _ => false
    })
  }

  /** What assembly did with a transaction */
  sealed trait Outcome
  object Outcome {
    /** In the candidate */
    case object Included extends Outcome
    /** Passed over at zero work: its L (record) or its no-script lower bound (no record) is above the room */
    case object NoRoom extends Outcome
    /** Passed over at zero work: too large for the size left */
    case object NoSize extends Outcome
    /** Passed over at zero work: its L is above what is left of its Z tier */
    case object NoBudget extends Outcome
    /** Evicted at zero work, no refusal, no charge: its record is above its L (Rg), so it cannot be measured fairly */
    case object Unmeasurable extends Outcome
    /** Passed over at zero work: spends or reads an output of a transaction left out of this candidate */
    case object Dependent extends Outcome
    /** Not looked at: this node evicted it already for this parent */
    case object OwnEviction extends Outcome
    /** Evicted as upstream (an input or data input missing, or a double spend); no refusal, no charge */
    case object Missing extends Outcome
    /** No record: valid, above the room, within L. Kept, its cost recorded, charged */
    case object KeptAboveRoom extends Outcome
    /** With a record: over L. Evicted, its inputs refused, charged */
    case object CostLiar extends Outcome
    /** With a record: invalid within L (script false or another rule). Evicted, its inputs refused, charged */
    case object Failed extends Outcome
    /** No record: over L or invalid. Evicted, no refusal, charged */
    case object PolicyBreak extends Outcome
    /** Dropped from the end because the fee transaction built at the end failed (no case of it is known) */
    case object DroppedForFeeTx extends Outcome
  }

  /** Charges to Z, tier by tier */
  final class ZBudget {
    private val used = Array.fill(ZTiers.size)(0L)

    def tierOf(limit: Long): Int = ZTiers.indexWhere { case (upTo, _) => limit <= upTo }

    def left(tier: Int): Long = ZTiers(tier)._2 - used(tier)

    def charge(tier: Int, amount: Long): Unit = used(tier) += math.max(0L, amount)

    def total: Long = used.sum

    def usedIn(tier: Int): Long = used(tier)
  }

  /** A pool transaction and its record under the current key, if any */
  final case class PoolTx(tx: ErgoTransaction, record: Option[Int])

  /**
    * Result of one assembly pass.
    *
    * @param txs            - the candidate's transactions, the fee transaction last
    * @param invalid        - transactions to evict (and whose ids to cache), mandatory ones included
    * @param refusedBoxIds  - inputs to refuse at admission (cost liars and failures with a record)
    * @param records        - costs this pass measured, by transaction id
    * @param outcomes       - per pool transaction: outcome, charge to Z and abort limit L
    * @param zUsed          - total charged to Z
    * @param passedOverForZ - some transaction was passed over because its Z tier ran short
    */
  final case class Result(txs: Seq[ErgoTransaction],
                          invalid: Seq[ModifierId],
                          refusedBoxIds: Seq[ModifierId],
                          records: Map[ModifierId, Int],
                          outcomes: Seq[(ModifierId, Outcome, Long, Long)],
                          zUsed: Long,
                          passedOverForZ: Boolean) {
    /** Some pool transaction was evicted */
    def evicted: Boolean = outcomes.exists { case (_, o, _, _) =>
      o == Outcome.CostLiar || o == Outcome.Failed || o == Outcome.PolicyBreak || o == Outcome.Missing
    }

    def outcomeOf(id: ModifierId): Option[Outcome] = outcomes.collectFirst { case (i, o, _, _) if i == id => o }
  }

  private def inputsPresent(tx: ErgoTransaction, s: UtxoStateReader): Boolean =
    tx.inputs.forall(inp => s.boxById(inp.boxId).isDefined)

  private def dataInputsPresent(tx: ErgoTransaction, s: UtxoStateReader): Boolean =
    tx.dataInputs.forall(inp => s.boxById(inp.boxId).isDefined)

  private def outputIdsOf(tx: ErgoTransaction): Seq[ModifierId] = tx.outputs.map(b => bytesToId(b.id))

  /**
    * One YOLO assembly pass (switch on only; `CandidateGenerator.createCandidate`).
    *
    * Mandatory transactions (`mandatoryTxs`: the emission tx, prioritized txs and the miner's own rent claim, in that
    * order) are validated as upstream, with the full budget, are never charged and never penalized. One that is
    * invalid is reported invalid; one that does not fit ends assembly, as upstream, except one in `droppableTxIds`
    * (the rent claim), which is left out.
    *
    * Pool transactions, in rank order, are passed over at zero work, validated with an abort limit, included,
    * evicted, or evicted with their inputs refused, as `Outcome` says. The fee transaction is
    * reserved by its bound and built once at the end; if it fails, pool transactions are dropped from the end
    * until it is valid. `excluded` are pool transactions this node already evicted for the same parent.
    *
    * @param b          - assembly budget B (`budget` of the upcoming context's parameters)
    * @param relayInUse - relay limit in use (`relayLimitInUse`), the abort limit of a transaction with no record
    */
  def collect(minerPk: ProveDlog,
              us: UtxoStateReader,
              upcomingContext: UpcomingStateContext,
              b: Int,
              relayInUse: Int,
              mandatoryTxs: Seq[ErgoTransaction],
              droppableTxIds: Set[ModifierId],
              poolTxs: Seq[PoolTx],
              excluded: Set[ModifierId] = Set.empty): Result = {

    val params = upcomingContext.currentParameters
    val chainSettings = upcomingContext.chainSettings
    val maxBlockSize = params.maxBlockSize
    val currentHeight = us.stateContext.currentHeight
    val minerPropSize =
      ErgoTreePredef.rewardOutputScript(chainSettings.monetary.minerRewardDelay, minerPk).bytes.length

    log.info(s"Assembling a YOLO block candidate for block #${upcomingContext.currentHeight} from " +
      s"${mandatoryTxs.size} mandatory and ${poolTxs.size} pool transactions available")

    val plainVerifier = ErgoInterpreter(params)
    val verifier = new YoloAssemblyVerifier(params)

    val included = mutable.ArrayBuffer.empty[CostedTransaction]
    var includedCost = 0L
    var includedSize = 0L
    var feeBoxes = 0
    var feeTokens = 0
    var feeCollection = 0L
    var stateWithTxs: UtxoStateReader = us
    val invalid = mutable.ArrayBuffer.empty[ModifierId]
    val refused = mutable.LinkedHashSet.empty[ModifierId]
    val records = mutable.LinkedHashMap.empty[ModifierId, Int]
    val outcomes = mutable.ArrayBuffer.empty[(ModifierId, Outcome, Long, Long)]
    val skippedOutputs = mutable.HashSet.empty[ModifierId]
    val z = new ZBudget
    var passedOverForZ = false

    def feeOutputsOf(tx: ErgoTransaction) = tx.outputCandidates.filter(YoloFeeOutputPolicy.isFeeOutput(_, chainSettings))

    // what including `tx` would leave as room for its own cost, and for its size
    def roomFor(tx: ErgoTransaction, f: Long): (Long, Long) = {
      val fees = feeOutputsOf(tx)
      val boxes = feeBoxes + fees.size
      val tokens = feeTokens + fees.map(_.additionalTokens.size).sum
      val costRoom = b - 1L - includedCost - feeTxCostBound(params, boxes, feeCollection + f)
      val sizeRoom = maxBlockSize - 1L - includedSize - feeTxSizeBound(minerPropSize, boxes, tokens)
      costRoom -> sizeRoom
    }

    def include(tx: ErgoTransaction, cost: Int, f: Long): Unit = {
      val fees = feeOutputsOf(tx)
      included += (tx -> cost)
      includedCost += cost
      includedSize += tx.size
      feeBoxes += fees.size
      feeTokens += fees.map(_.additionalTokens.size).sum
      feeCollection += f
      stateWithTxs = us.withTransactions(included.map(_._1).toSeq)
    }

    def current: Seq[ErgoTransaction] = included.map(_._1).toSeq

    def dependsOnSkipped(tx: ErgoTransaction): Boolean = skippedOutputs.nonEmpty && (
      tx.inputs.exists(i => skippedOutputs.contains(bytesToId(i.boxId))) ||
        tx.dataInputs.exists(d => skippedOutputs.contains(bytesToId(d.boxId))))

    def passOver(tx: ErgoTransaction): Unit = skippedOutputs ++= outputIdsOf(tx)

    // mandatory transactions, as upstream: an invalid one is reported, one that does not fit ends assembly unless
    // it is droppable
    @tailrec
    def mandatoryLoop(txs: List[ErgoTransaction]): Boolean = txs match {
      case Nil => true
      case tx :: tail =>
        if (dependsOnSkipped(tx)) {
          passOver(tx)
          mandatoryLoop(tail)
        } else if (!inputsPresent(tx, stateWithTxs) || doublespend(current, tx)) {
          log.debug(s"Mandatory transaction ${tx.id} double-spending or spending non-existing inputs")
          invalid += tx.id
          mandatoryLoop(tail)
        } else {
          stateWithTxs.validateWithCost(tx, upcomingContext, b, Some(plainVerifier)) match {
            case Success(cost) =>
              val f = YoloFeeOutputPolicy.feeCollectionCost(tx, chainSettings, params)
              val (costRoom, sizeRoom) = roomFor(tx, f)
              if (cost <= costRoom && tx.size <= sizeRoom) {
                include(tx, cost, f)
                mandatoryLoop(tail)
              } else if (droppableTxIds.contains(tx.id)) {
                log.info(s"Leaving out transaction ${tx.id} (cost $cost, size ${tx.size}): it does not fit, " +
                  s"room $costRoom, size room $sizeRoom")
                passOver(tx)
                mandatoryLoop(tail)
              } else {
                log.debug(s"Finishing block assembly at mandatory transaction ${tx.id} (cost $cost), room $costRoom")
                false
              }
            case Failure(e) =>
              log.info(s"Not included mandatory transaction ${tx.id} due to ${e.getMessage}")
              invalid += tx.id
              mandatoryLoop(tail)
          }
        }
    }

    def poolStep(p: PoolTx): Unit = {
      val tx = p.tx
      val f = YoloFeeOutputPolicy.feeCollectionCost(tx, chainSettings, params)
      val limit = abortLimit(p.record, f, b, relayInUse)
      def out(o: Outcome, charge: Long = 0L): Unit = outcomes += ((tx.id, o, charge, limit))
      val (costRoom, sizeRoom) = roomFor(tx, f)
      val tier = z.tierOf(limit)
      if (excluded.contains(tx.id)) {
        out(Outcome.OwnEviction)
      } else if (dependsOnSkipped(tx)) {
        passOver(tx); out(Outcome.Dependent)
      } else if (p.record.exists(_ > limit)) {
        invalid += tx.id
        passOver(tx); out(Outcome.Unmeasurable)
      } else if (p.record.fold(noScriptLowerBound(tx, params))(_ => limit) > costRoom) {
        passOver(tx); out(Outcome.NoRoom)
      } else if (tx.size > sizeRoom) {
        passOver(tx); out(Outcome.NoSize)
      } else if (limit > z.left(tier)) {
        passedOverForZ = true
        passOver(tx); out(Outcome.NoBudget)
      } else if (!inputsPresent(tx, stateWithTxs) || doublespend(current, tx) || !dataInputsPresent(tx, stateWithTxs)) {
        log.debug(s"Transaction ${tx.id} double-spending or spending or reading non-existing boxes")
        invalid += tx.id
        out(Outcome.Missing)
      } else {
        // validated in a copy of the upcoming context whose block cost limit is L: the result is the cost
        val atLimit = upcomingContext.copy(currentParameters = params.withBlockCost(math.max(0L, limit).toInt))(chainSettings)
        verifier.reset()
        val result = Try {
          tx.statelessValidity().flatMap { _ =>
            val boxes = tx.inputs.flatMap(i => stateWithTxs.boxById(i.boxId))
            val dataBoxes = tx.dataInputs.flatMap(i => stateWithTxs.boxById(i.boxId))
            tx.statefulValidity(boxes, dataBoxes, atLimit, 0L)(verifier)
          }
        }.flatten
        result match {
          case Success(cost) =>
            records.put(tx.id, math.max(cost, p.record.getOrElse(0)))
            if (cost <= costRoom) {
              include(tx, cost, f)
              out(Outcome.Included)
            } else {
              // only without a record: with one, L is within the room
              z.charge(tier, cost.toLong)
              passOver(tx)
              out(Outcome.KeptAboveRoom, cost.toLong)
            }
          case Failure(e) =>
            val overLimit = isOverLimit(e, verifier.failure)
            z.charge(tier, limit)
            invalid += tx.id
            val outcome = (p.record, overLimit) match {
              case (Some(_), true) => Outcome.CostLiar
              case (Some(_), false) => Outcome.Failed
              case (None, _) => Outcome.PolicyBreak
            }
            if (p.record.isDefined) refused ++= tx.inputs.map(i => bytesToId(i.boxId))
            log.debug(s"Transaction ${tx.id}: $outcome at abort limit $limit (record ${p.record}): " +
              s"${verifier.failure.map(_.getMessage).getOrElse(e.getMessage)}")
            out(outcome, math.max(0L, limit))
        }
      }
    }

    val poolStart = if (mandatoryLoop(mandatoryTxs.toList)) {
      val start = included.size
      poolTxs.foreach(poolStep)
      start
    } else {
      included.size
    }

    // the fee transaction, built and validated once; pool transactions dropped from the end while it fails
    @tailrec
    def withFeeTx(txs: Seq[CostedTransaction]): Seq[ErgoTransaction] = {
      val blockTxs = txs.map(_._1)
      val newBoxes = blockTxs.flatMap(_.outputs)
      val ok: Option[Seq[ErgoTransaction]] = CandidateGenerator.collectFees(currentHeight, blockTxs, minerPk,
        upcomingContext) match {
        case None =>
          if (correctLimits(txs, b, maxBlockSize)) Some(blockTxs) else None
        case Some(feeTx) =>
          val boxesToSpend = feeTx.inputs.flatMap(i => newBoxes.find(bx => java.util.Arrays.equals(bx.id, i.boxId)))
          feeTx.statefulValidity(boxesToSpend, IndexedSeq(), upcomingContext)(plainVerifier) match {
            case Success(cost) if correctLimits((feeTx -> cost) +: txs, b, maxBlockSize) => Some(blockTxs :+ feeTx)
            case other =>
              log.error(s"YOLO fee transaction does not fit or is invalid beside ${txs.size} transactions: $other")
              None
          }
      }
      ok match {
        case Some(res) => res
        case None if txs.size > poolStart =>
          outcomes += ((txs.last._1.id, Outcome.DroppedForFeeTx, 0L, 0L))
          withFeeTx(txs.init)
        case None =>
          log.error("YOLO fee transaction fails beside the mandatory transactions alone; candidate without it")
          blockTxs
      }
    }

    val txs = withFeeTx(included.toSeq)
    val counts = outcomes.groupBy(_._2).map { case (o, s) => s"$o ${s.size}" }.mkString(", ")
    log.info(s"YOLO assembly for block #${upcomingContext.currentHeight}: ${txs.size} transactions; pool outcomes: " +
      s"$counts; Z used ${z.total} (tiers ${ZTiers.indices.map(z.usedIn).mkString("/")}); " +
      s"${refused.size} inputs refused")
    Result(txs, invalid.distinct.toSeq, refused.toSeq, records.toMap, outcomes.toSeq, z.total, passedOverForZ)
  }

}

/**
  * What a mining node remembers between candidates (YOLO switch on, one instance per `CandidateGenerator`):
  *  - the costs it measured at assembly, by transaction id and record key (a record never goes down under
  *    the same key), pruned to the pool;
  *  - the transactions it evicted for the current parent, so a rerun on a stale pool snapshot does not look at them
  *    again, and how many passes for that parent ended short of Z with an eviction.
  * Not thread-safe: used by the generator actor only.
  */
final class YoloAssemblyMemory {
  private val records = mutable.HashMap.empty[ModifierId, (Int, Int)]
  private var parent: Option[ModifierId] = None
  private val evicted = mutable.HashSet.empty[ModifierId]
  private var zPasses = 0
  private var passes = 0

  /** Start tracking `parentId`; evictions and the pass counts of an earlier parent are forgotten */
  def forParent(parentId: Option[ModifierId]): Unit = if (parentId != parent) {
    parent = parentId
    evicted.clear()
    zPasses = 0
    passes = 0
  }

  /** Assembly passes run for the current parent */
  def passesForParent: Int = passes

  def notePass(): Unit = passes += 1

  def record(id: ModifierId, key: Int): Option[Int] = records.get(id).collect { case (c, k) if k == key => c }

  def keep(id: ModifierId, cost: Int, key: Int): Unit = {
    val c = record(id, key).fold(cost)(math.max(_, cost))
    records.put(id, c -> key)
  }

  /** Keep only the records of `ids` (the pool) */
  def prune(ids: Set[ModifierId]): Unit = records.retain((id, _) => ids.contains(id))

  def evictedForParent: Set[ModifierId] = evicted.toSet

  def noteEvicted(ids: Seq[ModifierId]): Unit = evicted ++= ids

  def zPassesForParent: Int = zPasses

  def noteZPass(): Unit = zPasses += 1

  def recordCount: Int = records.size
}
