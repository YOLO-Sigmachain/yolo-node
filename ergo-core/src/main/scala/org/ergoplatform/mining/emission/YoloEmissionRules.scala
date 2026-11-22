package org.ergoplatform.mining.emission

/**
  * Treasury, LP and miner parts of one emission spend, in nanocoins. They always sum to the amount split.
  *
  * @param treasury - value of the treasury share output
  * @param lp       - value of the LP share output
  * @param miner    - value of the miner output (the residual)
  */
case class YoloEmissionSplit(treasury: Long, lp: Long, miner: Long) {

  /** Total of the three parts: the block reward on the normal path, the box value on the terminal path. */
  def total: Long = treasury + lp + miner

}

/**
  * YOLO (SigmaChain) emission curve and split, in integer nanocoin arithmetic exactly as the emission contract in
  * the yolo-contracts repo (`01-emission-tests/emission.es` v1.4) computes them. Used in place of Ergo's `EmissionRules`
  * when `ergo.chain.yolo.enabled = true`; Ergo's rules in `ChainSettings.emissionRules` are left as they are.
  *
  * Curve: `halvings = height / BlocksPerHalving` (integer division, so height 1,051,200 is already in the second
  * arm); 75 YOLO for halvings <= 0, then 37.5, 18.75, 9.375, 4.6875 and 2.34375 YOLO for halvings 1 to 5, then the
  * 1.5 YOLO floor from height 6,307,200. Split on the normal path: treasury `reward * 4 / 100`, LP
  * `reward * 3 / 100`, miner the residual. On the terminal path (box value at most the reward) the same split is
  * taken on the box value, and the emission NFT is burned.
  *
  * The genesis emission box is created at height 0 and first spent at height 1, so heights 1 to h are paid by the
  * time the spend at height h is done.
  *
  * @param genesisValue - value of the genesis emission box, in nanocoins (`ergo.chain.yolo.emission.genesisValue`)
  */
class YoloEmissionRules(val genesisValue: Long) {
  import YoloEmissionRules._

  require(genesisValue > 0, s"YOLO genesis emission value must be positive, got $genesisValue")

  /**
    * Height of the terminal emission spend: the first height h >= 1 at which the emission box value before the
    * spend is at most `rewardAtHeight(h)`. At that height the whole box value is split by `terminalSplit` and the
    * NFT burned; from the next height on there is no emission transaction. For the 177,291,250 YOLO genesis value
    * it is 21,023,916, where the box holds a last 1.0 YOLO (less than the 1.5 YOLO reward).
    *
    * A Long, as for a large genesis value the tail can outrun the Int height range.
    */
  lazy val terminalHeight: Long = {
    // walk the arms: inside an arm the reward is a constant r, and the box holds `before - r * (h - start)` just
    // before the spend at height h
    @annotation.tailrec
    def loop(arm: Int, before: Long): Long = {
      val start: Long = if (arm == 0) 1L else arm.toLong * BlocksPerHalving
      val r = if (arm < ExplicitArms) InitialReward >> arm else MinReward
      // smallest k >= 0 with before - r * k <= r
      val k: Long = if (before <= r) 0L else (before - r + r - 1) / r
      val endExclusive: Long = (arm.toLong + 1) * BlocksPerHalving
      if (arm >= ExplicitArms || start + k < endExclusive) {
        start + k
      } else {
        loop(arm + 1, before - r * (endExclusive - start))
      }
    }
    loop(0, genesisValue)
  }

  /**
    * Value of the emission box once the emission spend at `height` is done: the genesis value for heights <= 0,
    * the genesis value less every reward paid at heights 1 to `height` before the terminal height, and 0 from the
    * terminal height on (the box is gone).
    *
    * @param height - block height
    * @return remaining value, in nanocoins
    */
  def remainingAfter(height: Long): Long = {
    if (height <= 0) {
      genesisValue
    } else if (height >= terminalHeight) {
      0L
    } else {
      genesisValue - paidThrough(height)
    }
  }

  /**
    * Value of the emission box spent at `height` (the box created by the spend at `height - 1`), or 0 when there is
    * no emission box to spend at that height.
    */
  def boxValueAt(height: Long): Long = if (height < 1) 0L else remainingAfter(height - 1)

  /**
    * Whether the emission spend at `height` takes the terminal path for the YOLO chain started from `genesisValue`.
    */
  def isTerminalHeight(height: Long): Boolean = height == terminalHeight

}

object YoloEmissionRules {

  /** One YOLO in nanocoins. */
  val CoinsInOneYolo: Long = 1000000000L

  /** emission.es `blocksPerHalving`: one 365-day year at 30 s blocks. */
  val BlocksPerHalving: Int = 1051200

  /** emission.es `initialReward`: 75 YOLO. */
  val InitialReward: Long = 75L * CoinsInOneYolo

  /** emission.es `minReward`: the 1.5 YOLO tail floor. */
  val MinReward: Long = 1500000000L

  /** Number of halving arms paid before the floor (halvings 0 to 5). */
  val ExplicitArms: Int = 6

  /** Treasury share, in percent of the amount split (emission.es `treasuryReward`). */
  val TreasuryPercent: Long = 4L

  /** LP share, in percent of the amount split (emission.es `lpReward`). */
  val LpPercent: Long = 3L

  /** First height paid at the floor: 6 * 1,051,200 = 6,307,200. */
  val FloorStartHeight: Int = ExplicitArms * BlocksPerHalving

  /**
    * Block reward at `height`, as emission.es `blockReward`: `halvings = height / BlocksPerHalving`, 75 YOLO for
    * halvings <= 0, `InitialReward / 2^halvings` for halvings 1 to 5, then `MinReward`, never below `MinReward`.
    */
  def rewardAtHeight(height: Long): Long = {
    val halvings = height / BlocksPerHalving
    val computed =
      if (halvings <= 0) InitialReward
      else if (halvings == 1) InitialReward / 2L
      else if (halvings == 2) InitialReward / 4L
      else if (halvings == 3) InitialReward / 8L
      else if (halvings == 4) InitialReward / 16L
      else if (halvings == 5) InitialReward / 32L
      else MinReward
    if (computed > MinReward) computed else MinReward
  }

  /**
    * Treasury, LP and miner parts of `amount`, as emission.es: treasury `amount * 4 / 100`, LP `amount * 3 / 100`,
    * miner `amount - treasury - LP`. Throws `ArithmeticException` on overflow, as the interpreter does.
    */
  def split(amount: Long): YoloEmissionSplit = {
    val treasury = Math.multiplyExact(amount, TreasuryPercent) / 100L
    val lp = Math.multiplyExact(amount, LpPercent) / 100L
    YoloEmissionSplit(treasury, lp, amount - treasury - lp)
  }

  /** Normal-path shares at `height`: the split of `rewardAtHeight(height)`. */
  def sharesAtHeight(height: Long): YoloEmissionSplit = split(rewardAtHeight(height))

  /** Treasury share at `height` on the normal path, in nanocoins. */
  def treasuryShareAtHeight(height: Long): Long = sharesAtHeight(height).treasury

  /** LP share at `height` on the normal path, in nanocoins. */
  def lpShareAtHeight(height: Long): Long = sharesAtHeight(height).lp

  /** Miner share at `height` on the normal path, in nanocoins. */
  def minerShareAtHeight(height: Long): Long = sharesAtHeight(height).miner

  /**
    * Terminal-path split of an emission box holding `boxValue`: the split is taken on the box value
    * (emission.es `termTreasury`, `termLP`, miner the rest).
    */
  def terminalSplit(boxValue: Long): YoloEmissionSplit = split(boxValue)

  /**
    * Whether an emission box holding `boxValue` is spent on the terminal path at `height`: emission.es
    * `SELF.value <= blockReward`. At equality the normal path would need a zero-value successor, which consensus
    * rejects, so the terminal path is the only valid one.
    */
  def isTerminal(boxValue: Long, height: Long): Boolean = boxValue <= rewardAtHeight(height)

  /**
    * Sum of the rewards at heights 1 to `height` (0 for heights below 1), ignoring exhaustion of the box.
    * No overflow for any Int height: the arms total under 2^58 and the tail under 2^62.
    */
  def paidThrough(height: Long): Long = {
    if (height < 1) {
      0L
    } else {
      var total = 0L
      var arm = 0
      while (arm <= ExplicitArms) {
        val start: Long = if (arm == 0) 1L else arm.toLong * BlocksPerHalving
        val end: Long = if (arm < ExplicitArms) (arm.toLong + 1) * BlocksPerHalving - 1 else Long.MaxValue
        val r = if (arm < ExplicitArms) InitialReward >> arm else MinReward
        if (height >= start) {
          val last = Math.min(height, end)
          total = Math.addExact(total, Math.multiplyExact(r, last - start + 1))
        }
        arm += 1
      }
      total
    }
  }

}
