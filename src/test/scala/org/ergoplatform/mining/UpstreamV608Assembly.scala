package org.ergoplatform.mining

import org.ergoplatform.mining.CandidateGenerator.{CostedTransaction, correctLimits, doublespend}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.state.{ErgoState, ErgoStateContext, UtxoStateReader}
import org.ergoplatform.sdk.wallet.Constants.MaxAssetsPerBox
import org.ergoplatform.settings.{Constants, Parameters}
import org.ergoplatform.wallet.interpreter.ErgoInterpreter
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, ErgoTreePredef, Input}
import scorex.util.{ModifierId, bytesToId}
import sigma.Extensions.{ArrayOps, CollBytesOps}
import sigma.ast.{ErgoTree, ShortConstant}
import sigma.ast.syntax.ErgoBoxRType
import sigma.data.ProveDlog
import sigma.interpreter.{ContextExtension, ProverResult}
import sigma.{Coll, Colls}

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success}

/**
  * Reference copies of upstream Ergo v6.0.8 (`c1a69f65d`) block assembly code, kept here so that tests can pin the
  * switch-off behaviour of the node against upstream itself rather than against the new code's own defaults:
  * - `collectTxs` and the fee part of `collectRewards` (`collectFees`) from `CandidateGenerator.scala`;
  * - `buildClaim` from `StorageRentClaimBuilder.scala` (logging dropped, comments shortened).
  *
  * The logic is copied as is; only logging calls are removed. Shared helpers that the YOLO work did not change
  * (`correctLimits`, `doublespend`, `ErgoState.newBoxes`) are called, not copied.
  */
object UpstreamV608Assembly {

  private def inputsNotSpent(tx: ErgoTransaction, s: UtxoStateReader): Boolean =
    tx.inputs.forall(inp => s.boxById(inp.boxId).isDefined)

  /** v6.0.8 `CandidateGenerator.collectFees`: `collectRewards(None, ...)`, which builds the fee transaction only */
  def collectFees(currentHeight: Int,
                  txs: Seq[ErgoTransaction],
                  minerPk: ProveDlog,
                  stateContext: ErgoStateContext): Option[ErgoTransaction] = {
    val chainSettings = stateContext.chainSettings
    val propositionBytes = chainSettings.monetary.feePropositionBytes
    val emission = chainSettings.emissionRules
    val nextHeight = currentHeight + 1
    val minerProp =
      ErgoTreePredef.rewardOutputScript(emission.settings.minerRewardDelay, minerPk)

    val inputs = txs.flatMap(_.inputs)
    val feeBoxes: Seq[ErgoBox] = ErgoState
      .newBoxes(txs)
      .filter(b => java.util.Arrays.equals(b.propositionBytes, propositionBytes) && !inputs.exists(i => java.util.Arrays.equals(i.boxId, b.id)))
    if (feeBoxes.nonEmpty) {
      val feeAmount = feeBoxes.map(_.value).sum
      val feeAssets =
        feeBoxes.toArray.toColl.flatMap(_.additionalTokens).take(MaxAssetsPerBox)
      val inputs = feeBoxes.map(b => new Input(b.id, ProverResult.empty))
      val minerBox =
        new ErgoBoxCandidate(feeAmount, minerProp, nextHeight, feeAssets, Map())
      Some(ErgoTransaction(inputs.toIndexedSeq, IndexedSeq(), IndexedSeq(minerBox)))
    } else {
      None
    }
  }

  /** v6.0.8 `CandidateGenerator.collectTxs` */
  def collectTxs(minerPk: ProveDlog,
                 maxBlockCost: Int,
                 maxBlockSize: Int,
                 us: UtxoStateReader,
                 upcomingContext: ErgoStateContext,
                 transactions: Seq[ErgoTransaction]): (Seq[ErgoTransaction], Seq[ModifierId]) = {

    val currentHeight = us.stateContext.currentHeight

    val verifier: ErgoInterpreter = ErgoInterpreter(upcomingContext.currentParameters)

    @tailrec
    def loop(mempoolTxs: Iterable[ErgoTransaction],
             acc: Seq[CostedTransaction],
             lastFeeTx: Option[CostedTransaction],
             invalidTxs: Seq[ModifierId]): (Seq[ErgoTransaction], Seq[ModifierId]) = {
      val currentCosted = acc ++ lastFeeTx
      def current: Seq[ErgoTransaction] = currentCosted.map(_._1)

      val stateWithTxs = us.withTransactions(current)

      mempoolTxs.headOption match {
        case Some(tx) =>
          if (!inputsNotSpent(tx, stateWithTxs) || doublespend(current, tx)) {
            loop(mempoolTxs.tail, acc, lastFeeTx, invalidTxs :+ tx.id)
          } else {
            stateWithTxs.validateWithCost(
              tx,
              upcomingContext,
              maxBlockCost,
              Some(verifier)
            ) match {
              case Success(costConsumed) =>
                val newTxs = acc :+ (tx -> costConsumed)
                val newBoxes = newTxs.flatMap(_._1.outputs)

                collectFees(currentHeight, newTxs.map(_._1), minerPk, upcomingContext) match {
                  case Some(feeTx) =>
                    val boxesToSpend = feeTx.inputs.flatMap(i =>
                      newBoxes.find(b => java.util.Arrays.equals(b.id, i.boxId))
                    )
                    feeTx.statefulValidity(boxesToSpend, IndexedSeq(), upcomingContext)(verifier) match {
                      case Success(cost) =>
                        val blockTxs: Seq[CostedTransaction] = (feeTx -> cost) +: newTxs
                        if (correctLimits(blockTxs, maxBlockCost, maxBlockSize)) {
                          loop(mempoolTxs.tail, newTxs, Some(feeTx -> cost), invalidTxs)
                        } else {
                          current -> invalidTxs
                        }
                      case Failure(_) =>
                        current -> invalidTxs
                    }
                  case None =>
                    val blockTxs: Seq[CostedTransaction] = newTxs ++ lastFeeTx.toSeq
                    if (correctLimits(blockTxs, maxBlockCost, maxBlockSize)) {
                      loop(mempoolTxs.tail, blockTxs, lastFeeTx, invalidTxs)
                    } else {
                      current -> invalidTxs
                    }
                }
              case Failure(_) =>
                loop(mempoolTxs.tail, acc, lastFeeTx, invalidTxs :+ tx.id)
            }
          }
        case None =>
          current -> invalidTxs
      }
    }

    loop(transactions, Seq.empty, None, Seq.empty)
  }

  private val DummyTxId: ModifierId = bytesToId(Array.fill(32)(0.toByte))

  private def boxSize(candidate: ErgoBoxCandidate, outputIndex: Short): Int =
    candidate.toBox(DummyTxId, outputIndex).bytes.length

  private def dustLimit(candidate: ErgoBoxCandidate, outputIndex: Short, parameters: Parameters): Long =
    parameters.minValuePerByte.toLong * boxSize(candidate, outputIndex)

  private def p2pkCandidate(value: Long, minerTree: ErgoTree, currentHeight: Int): ErgoBoxCandidate =
    new ErgoBoxCandidate(value, minerTree, currentHeight, Colls.emptyColl, Map.empty)

  /** v6.0.8 `StorageRentClaimBuilder.buildClaim` (MaxClaims = 100) */
  def buildClaim(eligible: Seq[ErgoBox],
                 currentHeight: Int,
                 parameters: Parameters,
                 minerPk: ProveDlog,
                 reemissionTokenIdOpt: Option[ModifierId],
                 tokenWhitelist: Set[ModifierId],
                 onPermanentlySkipped: ErgoBox => Unit = _ => ()): Option[ErgoTransaction] = {

    val minerTree = ErgoTree.fromSigmaBoolean(minerPk)
    val claimed = ArrayBuffer.empty[(ErgoBox, Boolean, ErgoBoxCandidate)]
    var sweptFees = 0L

    eligible.iterator.takeWhile(_ => claimed.length < 100).foreach { box =>
      val age = currentHeight - box.creationHeight
      val oldEnough = age >= Constants.StoragePeriod
      val carriesReemissionToken = reemissionTokenIdOpt.exists(box.tokens.contains(_))
      val minValue = parameters.minValuePerByte * box.bytes.length
      val aboveMinValue = minValue > 0 && box.value > minValue.toLong
      if (!oldEnough) {
        // transient
      } else if (carriesReemissionToken || !aboveMinValue) {
        onPermanentlySkipped(box)
      } else {
        val storageFee = parameters.storageFeeFactor * box.bytes.length
        if (storageFee > 0) {
          val afterFee = box.value - storageFee
          if (afterFee > 0) {
            val outputIndex = claimed.length.toShort
            var recreatedBox = new ErgoBoxCandidate(afterFee, box.ergoTree,
              currentHeight, box.additionalTokens, box.additionalRegisters)
            var dust = dustLimit(recreatedBox, outputIndex, parameters)
            while (recreatedBox.value < dust) {
              recreatedBox = new ErgoBoxCandidate(dust, box.ergoTree, currentHeight,
                box.additionalTokens, box.additionalRegisters)
              dust = dustLimit(recreatedBox, outputIndex, parameters)
            }
            if (recreatedBox.value <= box.value && boxSize(recreatedBox, outputIndex) <= ErgoBox.MaxBoxSize) {
              sweptFees += box.value - recreatedBox.value
              claimed += ((box, true, recreatedBox))
            } else {
              onPermanentlySkipped(box)
            }
          } else {
            val salvagedTokens: Coll[(ErgoBox.TokenId, Long)] =
              if (tokenWhitelist.isEmpty) {
                Colls.emptyColl
              } else {
                box.additionalTokens.filter(t => tokenWhitelist.contains(t._1.toModifierId))
              }
            val burnOutput = new ErgoBoxCandidate(box.value, minerTree, currentHeight, salvagedTokens, Map.empty)
            if (box.value >= dustLimit(burnOutput, claimed.length.toShort, parameters) &&
              boxSize(burnOutput, claimed.length.toShort) <= ErgoBox.MaxBoxSize) {
              claimed += ((box, false, burnOutput))
            } else {
              onPermanentlySkipped(box)
            }
          }
        } else {
          onPermanentlySkipped(box)
        }
      }
    }

    if (claimed.isEmpty) {
      None
    } else {
      val feeDust = dustLimit(p2pkCandidate(0L, minerTree, currentHeight), 0, parameters)
      val dropRecreations = sweptFees > 0 && sweptFees < feeDust
      val finalClaimed = if (dropRecreations) claimed.filter(!_._2) else claimed

      if (finalClaimed.isEmpty) {
        None
      } else {
        val feeOutputs =
          if (!dropRecreations && sweptFees > 0) IndexedSeq(p2pkCandidate(sweptFees, minerTree, currentHeight))
          else IndexedSeq.empty
        val outputCandidates = finalClaimed.toIndexedSeq.map(_._3) ++ feeOutputs

        val inputs = finalClaimed.toIndexedSeq.zipWithIndex.map { case ((box, _, _), outputIndex) =>
          Input(box.id, ProverResult(
            Array.emptyByteArray,
            ContextExtension(Map(Constants.StorageIndexVarId -> ShortConstant(outputIndex.toShort)))
          ))
        }
        Some(ErgoTransaction(inputs, IndexedSeq.empty, outputCandidates))
      }
    }
  }

}
