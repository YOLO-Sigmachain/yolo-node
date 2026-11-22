package org.ergoplatform.nodeView.mempool

import org.ergoplatform.ErgoBoxCandidate
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.state.{ErgoStateContext, UtxoStateReader}
import org.ergoplatform.settings.{ChainSettings, Parameters}
import org.ergoplatform.wallet.interpreter.ErgoInterpreter

import scala.util.{Failure, Success, Try}

/**
  * YOLO relay policy for fee outputs, the outputs locked by the chain's fee proposition, which the miner of the block
  * collects into one output of its fee transaction (`CandidateGenerator.collectRewards`). It is mempool and relay
  * policy only: block and transaction validity do not change.
  *
  * With the YOLO switch on (`ergo.chain.yolo.enabled`), a transaction is refused when:
  *  - one of its fee outputs carries a token. Wallets never need this, upstream fee collection burns tokens past
  *    100, and a token-carrying fee box can be left out of the fee transaction (`yoloFeeCollection`), so its value
  *    is not a fee any miner can be relied on to collect;
  *  - its own validation cost plus what its fee outputs add to the fee transaction (`feeCollectionCost`) is above
  *    the relay cost limit (`ergo.node.maxTransactionCost`). Without this, one relayed transaction with many small
  *    fee outputs costs little itself, but its fee transaction never fits in a block or is invalid.
  *
  * With the switch off, `validateWithCost` is exactly `UtxoStateReader.validateWithCost`.
  *
  * The relay cost limit is enforced in three places, and all three go through `validateWithCost`: mempool acceptance
  * (`ErgoMemPool.process`), the mempool cleanup's revalidation (`CleanupWorker.revalidate`) and the API check
  * (`ErgoBaseApiRoute.verifyTransaction`, behind POST /transactions, /transactions/bytes, /transactions/check,
  * /transactions/checkBytes and the wallet's send routes). Where no UTXO set is kept (digest mode), no cost is known
  * and only `checkTokens` applies.
  */
object YoloFeeOutputPolicy {

  /** A refusal by this policy, as opposed to a validation failure of the transaction */
  class Refusal(message: String) extends Exception(message)

  /**
    * Upper bound on the cost of the fee proposition's script for one input of the fee transaction. Measured: 89 at
    * the YOLO launch parameters, the same for every fee box (the script compares the fee transaction's one output
    * with the miner reward script, and reads neither the box's own value nor its tokens).
    */
  val FeeScriptCostBound: Int = 100

  /** Whether `out` is locked by the chain's fee proposition, compared as `collectRewards` compares it */
  def isFeeOutput(out: ErgoBoxCandidate, chainSettings: ChainSettings): Boolean =
    java.util.Arrays.equals(out.propositionBytes, chainSettings.monetary.feePropositionBytes)

  /**
    * Upper bound on what collecting `feeBox` adds to the validation cost of the fee transaction
    * (`ErgoTransaction.validateStateful`): one input (`inputCost`), the fee script (`FeeScriptCostBound`), and the
    * token terms. Validation charges `tokenAccessCost` once per token entry of the inputs, once per distinct id of the
    * inputs, once per token entry of the outputs and once per distinct id of the outputs
    * (`ErgoBoxAssetExtractor.totalAssetsAccessCost`); a token entry of a fee box adds at most one to each of the four
    * counts, so 4 x `tokenAccessCost` per entry is never below the real figure.
    */
  def feeBoxCollectionCost(feeBox: ErgoBoxCandidate, parameters: Parameters): Long =
    parameters.inputCost.toLong + FeeScriptCostBound + 4L * parameters.tokenAccessCost * feeBox.additionalTokens.size

  /** Upper bound on what the fee outputs of `tx` add to the fee transaction's validation cost */
  def feeCollectionCost(tx: ErgoTransaction, chainSettings: ChainSettings, parameters: Parameters): Long =
    tx.outputCandidates.filter(isFeeOutput(_, chainSettings)).map(feeBoxCollectionCost(_, parameters)).sum

  /**
    * Part of the fee transaction's validation cost that does not depend on its inputs: the interpreter init cost and
    * its one output. The fee transaction of a block costs at most this plus `feeBoxCollectionCost` of each fee box it
    * collects; `CandidateGenerator.YoloFeeTxCostAllowance` covers it.
    */
  def feeTxBaseCost(parameters: Parameters): Long =
    ErgoInterpreter.interpreterInitCost.toLong + parameters.outputCost

  /** With the switch on, a Refusal when a fee output of `tx` carries a token; always a Success with it off */
  def checkTokens(tx: ErgoTransaction, chainSettings: ChainSettings): Try[Unit] = {
    if (!chainSettings.yolo.enabled) {
      Success(())
    } else {
      tx.outputCandidates.zipWithIndex.find { case (out, _) =>
        isFeeOutput(out, chainSettings) && !out.additionalTokens.isEmpty
      } match {
        case Some((out, idx)) =>
          Failure(new Refusal(s"Mempool policy declines transaction ${tx.id}: fee output #$idx carries " +
            s"${out.additionalTokens.size} token(s); fee outputs must not carry tokens"))
        case None =>
          Success(())
      }
    }
  }

  /**
    * With the switch on, a Refusal when `txCost` (the transaction's own validation cost) plus `feeCollectionCost` is
    * above `costLimit`; always a Success with it off
    */
  def checkCost(tx: ErgoTransaction,
                txCost: Long,
                costLimit: Long,
                chainSettings: ChainSettings,
                parameters: Parameters): Try[Unit] = {
    if (!chainSettings.yolo.enabled) {
      Success(())
    } else {
      val feeCost = feeCollectionCost(tx, chainSettings, parameters)
      if (txCost + feeCost <= costLimit) {
        Success(())
      } else {
        val feeOutputs = tx.outputCandidates.count(isFeeOutput(_, chainSettings))
        Failure(new Refusal(s"Mempool policy declines transaction ${tx.id}: it costs $txCost and its $feeOutputs fee " +
          s"output(s) add up to $feeCost to the miner's fee transaction, together ${txCost + feeCost}, above the " +
          s"transaction cost limit $costLimit (ergo.node.maxTransactionCost)"))
      }
    }
  }

  /**
    * `state.validateWithCost(tx, context, costLimit, None)` and, with the switch on (`context.chainSettings`), this
    * policy: `checkTokens` before validation (no script is run for a refused transaction) and `checkCost` after it.
    * A refusal is a Failure with a `Refusal`; any other Failure is the validation's. With the switch off it is
    * exactly the validation.
    */
  def validateWithCost(state: UtxoStateReader,
                       tx: ErgoTransaction,
                       context: ErgoStateContext,
                       costLimit: Int): Try[Int] = {
    val chainSettings = context.chainSettings
    if (!chainSettings.yolo.enabled) {
      state.validateWithCost(tx, context, costLimit, None)
    } else {
      for {
        _ <- checkTokens(tx, chainSettings)
        cost <- state.validateWithCost(tx, context, costLimit, None)
        _ <- checkCost(tx, cost.toLong, costLimit.toLong, chainSettings, context.currentParameters)
      } yield cost
    }
  }

}
