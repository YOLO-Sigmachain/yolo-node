package org.ergoplatform.mining.difficulty

/**
  * Independent model of the YOLO difficulty rule (EIP-37 with a first-retarget guard and a difficulty floor),
  * written for the tests from the EIP-37 description. It calls no node code: no `DifficultyAdjustment`,
  * no `DifficultySerializer`, no `YoloSettings`. Integer semantics follow EIP-37 as deployed on Ergo mainnet
  * (truncating `BigInt` division, 10^9 fixed point in the regression, nBits normalisation after the clamp).
  *
  * A block is described by its height, timestamp (ms) and required difficulty only.
  */
object YoloDifficultyReference {

  case class Blk(height: Int, timestamp: Long, difficulty: BigInt)

  /** YOLO difficulty floor, as a literal (the node has it as `YoloSettings.MinDifficulty`). */
  val Floor: BigInt = BigInt(2)

  /** Regression window: last 8 epochs, so 9 epoch-end headers (Ergo `useLastEpochs`). */
  val UseLastEpochs: Int = 8

  /** Fixed point of the regression slope (Ergo `DifficultyAdjustment.PrecisionConstant`). */
  private val Precision: BigInt = BigInt(1000000000)

  /**
    * nBits normalisation of a non-negative difficulty: keep the 3 most significant bytes of its minimal
    * two's-complement big-endian form and zero the rest. For a non-negative number the top byte of those three
    * never has its high bit set, so the nBits sign-bit adjustment never applies.
    */
  def normalize(x: BigInt): BigInt = {
    require(x >= 0, s"negative difficulty $x")
    val size = x.toByteArray.length
    if (size <= 3) x else {
      val shift = 8 * (size - 3)
      (x >> shift) << shift
    }
  }

  /** YOLO guard: the block after `parentHeight` retargets only on an epoch boundary from parent height 2E on. */
  def isRetarget(parentHeight: Int, epochLength: Int): Boolean =
    parentHeight >= 2 * epochLength && parentHeight % epochLength == 0

  /** Heights of the epoch-end headers used by a retarget after `parentHeight` that exist (genesis is height 1). */
  def windowHeights(parentHeight: Int, epochLength: Int): Seq[Int] =
    (UseLastEpochs to 0 by -1).map(i => parentHeight - i * epochLength).filter(_ >= 1)

  /** Least-squares line through `points`, evaluated one epoch after the last point (EIP-37 predictive term). */
  private def regression(points: Seq[(Int, BigInt)], epochLength: Int): BigInt = {
    val n = BigInt(points.size)
    if (points.size == 1) {
      points.head._2
    } else {
      var sx = BigInt(0)
      var sy = BigInt(0)
      var sxy = BigInt(0)
      var sx2 = BigInt(0)
      points.foreach { case (x, y) =>
        sx += x
        sy += y
        sxy += BigInt(x) * y
        sx2 += BigInt(x) * x
      }
      val slope = (sxy * n - sx * sy) * Precision / (sx2 * n - sx * sx)
      val intercept = (sy * Precision - slope * sx) / n / Precision
      intercept + slope * (points.map(_._1).max + epochLength) / Precision
    }
  }

  /** Difficulty an epoch would have needed to take `blockIntervalMs` per block, from its start and end blocks. */
  private def epochDifficulty(start: Blk, end: Blk, blockIntervalMs: Long, epochLength: Int): BigInt =
    end.difficulty * blockIntervalMs * epochLength / (end.timestamp - start.timestamp)

  /**
    * EIP-37 retarget, unfloored.
    *
    * @param window            - epoch-end blocks, oldest first, at least 2
    * @param initialDifficulty - fallback of the predictive term when the regression is below 1
    */
  def eip37(window: Seq[Blk], epochLength: Int, blockIntervalMs: Long, initialDifficulty: BigInt): BigInt = {
    require(window.size >= 2, s"EIP-37 needs at least 2 blocks, got ${window.size}")
    val last = window.last.difficulty

    val predictive = if (window.head.timestamp >= window.last.timestamp) {
      normalize(window.head.difficulty)
    } else {
      val points = window.zip(window.tail).map { case (s, e) =>
        require(e.height - s.height == epochLength, s"window blocks ${s.height} and ${e.height} are not one epoch apart")
        e.height -> epochDifficulty(s, e, blockIntervalMs, epochLength)
      }
      val r = regression(points, epochLength)
      normalize(if (r >= 1) r else initialDifficulty)
    }
    val up = last * 3 / 2
    val down = last / 2
    def clamp(x: BigInt): BigInt = if (x > last) x.min(up) else x.max(down)

    val classic = epochDifficulty(window(window.size - 2), window.last, blockIntervalMs, epochLength)
    val average = (classic + clamp(predictive)) / 2
    normalize(clamp(average))
  }

  /** YOLO retarget: EIP-37 raised to the floor. */
  def yoloRetarget(window: Seq[Blk], epochLength: Int, blockIntervalMs: Long, initialDifficulty: BigInt): BigInt =
    eip37(window, epochLength, blockIntervalMs, initialDifficulty).max(Floor)

  /**
    * Required difficulty of the block after `chain.last` on YOLO.
    *
    * @param chain - the whole chain from genesis (height 1), in height order
    */
  def requiredAfter(chain: IndexedSeq[Blk], epochLength: Int, blockIntervalMs: Long, initialDifficulty: BigInt): BigInt = {
    val parent = chain.last
    require(chain.head.height == 1 && parent.height == chain.size, "chain must run from genesis without gaps")
    if (isRetarget(parent.height, epochLength)) {
      val window = windowHeights(parent.height, epochLength).map(h => chain(h - 1))
      yoloRetarget(window, epochLength, blockIntervalMs, initialDifficulty)
    } else {
      parent.difficulty
    }
  }

}
