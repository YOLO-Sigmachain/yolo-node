package org.ergoplatform.mining

import org.ergoplatform.ErgoBox.{R4, R5}
import org.ergoplatform.mining.emission.{YoloEmissionRules, YoloEmissionSplit}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.settings.YoloEmissionSettings
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, ErgoTreePredef, Input}
import sigma.Colls
import sigma.ast.ErgoTree
import sigma.data.ProveDlog
import sigma.interpreter.ProverResult

/**
  * YOLO (SigmaChain) coinbase builder, with the 2880-block coinbase miner delay: the emission transaction the
  * emission contract in the yolo-contracts repo (`01-emission-tests/emission.es` v1.4) accepts. Used by
  * `CandidateGenerator.collectRewards` in place of Ergo's emission transaction when `ergo.chain.yolo.enabled = true`.
  *
  * One input, the emission box, with an empty proof (the emission script needs no signature, as upstream). No data
  * inputs. Every output has creation height `nextHeight` (the contract's `HEIGHT`).
  *
  * Normal path (`box value > reward at nextHeight`), exactly 4 outputs:
  *   0. successor emission box: value `box value - reward`, the input's tree, exactly one token (the emission NFT,
  *      amount 1), R4 and R5 copied from the input, no other register;
  *   1. treasury share `reward * 4 / 100` to the treasury accumulation tree;
  *   2. LP share `reward * 3 / 100` to the LP accumulation tree;
  *   3. miner share (the residual) to `rewardOutputScript(CoinbaseMinerRewardDelay, minerPk)`.
  *
  * Terminal path (`box value <= reward at nextHeight`), exactly 3 outputs: treasury, LP and miner, split on the box
  * value; the emission NFT is burned (it is in no output).
  *
  * Share and miner outputs hold no tokens and no registers.
  */
object YoloEmissionTx {

  /**
    * Delay of the coinbase miner output, in blocks (24 h at 30 s), as emission.es pins it
    * (`rewardOutputScript(2880, minerPk)`). Deliberately not `monetary.minerRewardDelay`, which stays 720 for the fee
    * proposition and fee collection.
    */
  val CoinbaseMinerRewardDelay: Int = 2880

  /** Script of the coinbase miner output for `minerPk`: `rewardOutputScript(CoinbaseMinerRewardDelay, minerPk)`. */
  def minerOutputScript(minerPk: ProveDlog): ErgoTree =
    ErgoTreePredef.rewardOutputScript(CoinbaseMinerRewardDelay, minerPk)

  /**
    * Whether the spend of an emission box holding `boxValue` at `height` takes the terminal path
    * (emission.es `SELF.value <= blockReward`).
    */
  def isTerminal(boxValue: Long, height: Int): Boolean = YoloEmissionRules.isTerminal(boxValue, height)

  /**
    * Builds the YOLO emission transaction spending `emissionBox` in the block at `nextHeight`.
    *
    * @param emissionBox - the tracked emission box (`UtxoStateReader.emissionBoxOpt`)
    * @param nextHeight  - height of the block the transaction goes into
    * @param minerPk     - miner public key of that block's header (the contract binds the miner output to it)
    * @param emission    - YOLO emission inputs (`chainSettings.yolo.emission`), for the two accumulation trees
    * @return the normal-path (4 outputs) or terminal-path (3 outputs) transaction
    */
  def build(emissionBox: ErgoBox,
            nextHeight: Int,
            minerPk: ProveDlog,
            emission: YoloEmissionSettings): ErgoTransaction = {
    val reward = YoloEmissionRules.rewardAtHeight(nextHeight)

    def plain(value: Long, tree: ErgoTree): ErgoBoxCandidate =
      new ErgoBoxCandidate(value, tree, nextHeight, Colls.emptyColl, Map.empty)

    def sharesAndMiner(split: YoloEmissionSplit): IndexedSeq[ErgoBoxCandidate] = IndexedSeq(
      plain(split.treasury, emission.treasuryAccumulationTree),
      plain(split.lp, emission.lpAccumulationTree),
      plain(split.miner, minerOutputScript(minerPk)))

    val outputs = if (isTerminal(emissionBox.value, nextHeight)) {
      sharesAndMiner(YoloEmissionRules.terminalSplit(emissionBox.value))
    } else {
      val registers = emissionBox.additionalRegisters.filter { case (r, _) => r == R4 || r == R5 }
      val successor = new ErgoBoxCandidate(
        emissionBox.value - reward,
        emissionBox.ergoTree,
        nextHeight,
        emissionBox.additionalTokens.take(1),
        registers)
      successor +: sharesAndMiner(YoloEmissionRules.split(reward))
    }

    ErgoTransaction(IndexedSeq(new Input(emissionBox.id, ProverResult.empty)), IndexedSeq.empty, outputs)
  }

}
