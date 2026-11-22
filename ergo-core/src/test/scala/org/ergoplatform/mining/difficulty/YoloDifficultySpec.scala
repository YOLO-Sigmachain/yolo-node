package org.ergoplatform.mining.difficulty

import org.ergoplatform.mining.difficulty.YoloDifficultyReference.Blk
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.settings.{ChainSettings, YoloSettings}
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.scalacheck.Gen

import scala.concurrent.duration._

/**
  * YOLO difficulty rule in `DifficultyAdjustment`: the first-retarget guard and the
  * difficulty floor, checked against the independent model in `YoloDifficultyReference` and against the two
  * properties the external header relay relies on at a retarget (2D <= 3P, D >= normalise(P / 2)).
  */
class YoloDifficultySpec extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.generators.ErgoCoreGenerators._

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 3000)

  private val Epoch = YoloSettings.Eip37EpochLength
  private val IntervalMs: Long = 30.seconds.toMillis

  private val yoloChain: ChainSettings = chainSettings.copy(
    blockInterval = 30.seconds,
    epochLength = Epoch,
    eip37EpochLength = Some(Epoch),
    useLastEpochs = 8,
    yolo = YoloSettings(enabled = true))

  private val control = new DifficultyAdjustment(yoloChain)

  private lazy val baseHeader: Header = defaultHeaderGen.sample.get

  private def normalized(d: BigInt): BigInt =
    DifficultySerializer.decodeCompactBits(DifficultySerializer.encodeCompactBits(d))

  private def header(height: Int, timestamp: Long, difficulty: BigInt): Header =
    baseHeader.copy(height = height, timestamp = timestamp, nBits = DifficultySerializer.encodeCompactBits(difficulty))

  private def blk(h: Header): Blk = Blk(h.height, h.timestamp, h.requiredDifficulty)

  /** Epoch-end headers at heights `lastHeight - (n-1) * Epoch, ..., lastHeight`, each epoch lasting `epochMs(i)`. */
  private def window(lastHeight: Int, difficulties: Seq[BigInt], epochMs: Seq[Long], t0: Long = 1600000000000L): Seq[Header] = {
    require(difficulties.size == epochMs.size + 1)
    val heights = difficulties.indices.map(i => lastHeight - (difficulties.size - 1 - i) * Epoch)
    val timestamps = epochMs.scanLeft(t0)(_ + _)
    heights.indices.map(i => header(heights(i), timestamps(i), difficulties(i)))
  }

  property("the YOLO constants: floor 2, exactly representable in nBits; EIP-37 epoch 64") {
    YoloSettings.MinDifficulty shouldBe BigInt(2)
    YoloSettings.Eip37EpochLength shouldBe 64
    YoloSettings.MinDifficulty shouldBe YoloDifficultyReference.Floor
    normalized(YoloSettings.MinDifficulty) shouldBe YoloSettings.MinDifficulty
  }

  property("guard: retarget only at parent heights 128, 192, ... (EIP-37 epoch 64)") {
    // (parent height, YOLO retarget)
    val vectors = Seq(
      (0, false),
      (63, false),
      (64, false),
      (65, false),
      (127, false),
      (128, true),
      (129, false),
      (192, true),
      (256, true),
      (1024, true),
      (1088, true)
    )
    vectors.foreach { case (parentHeight, yolo) =>
      withClue(s"parent height $parentHeight: ") {
        control.yoloIsRetarget(parentHeight, Epoch) shouldBe yolo
      }
    }
    (0 to 20000).foreach { h =>
      control.yoloIsRetarget(h, Epoch) shouldBe YoloDifficultyReference.isRetarget(h, Epoch)
    }
  }

  property("guard: without it, the retarget after parent height 64 has one header and eip37Calculate throws") {
    // heights 0 and 64 are asked for, and height 0 does not exist (genesis height is 1)
    control.previousHeightsRequiredForRecalculation(65, Epoch) shouldBe Seq(0, 64)
    val only64 = Seq(header(64, 1600000000000L, BigInt(1000)))
    an[IllegalArgumentException] should be thrownBy control.eip37Calculate(only64, Epoch)
    // at parent height 128 two headers exist (64 and 128), so block 129 is the first retarget
    control.previousHeightsRequiredForRecalculation(129, Epoch) shouldBe Seq(0, 64, 128)
    YoloDifficultyReference.windowHeights(128, Epoch) shouldBe Seq(64, 128)
  }

  property("floor: an unfloored result of 1 (difficulty 2, slow epoch) becomes 2") {
    val w = window(2 * Epoch, Seq(BigInt(2), BigInt(2)), Seq(Epoch * IntervalMs * 10))
    control.eip37Calculate(w, Epoch) shouldBe BigInt(1)
    control.yoloEip37Calculate(w, Epoch) shouldBe BigInt(2)
    YoloDifficultyReference.eip37(w.map(blk), Epoch, IntervalMs, yoloChain.initialDifficulty) shouldBe BigInt(1)
    YoloDifficultyReference.yoloRetarget(w.map(blk), Epoch, IntervalMs, yoloChain.initialDifficulty) shouldBe BigInt(2)
  }

  property("floor: an unfloored result of 0 (difficulty 1, slow epoch) becomes 2") {
    // difficulty 1 never occurs on YOLO (genesis >= 2 by the start-up check, every retarget >= 2 by the floor);
    // this is the dead state the floor exists for
    val w = window(2 * Epoch, Seq(BigInt(1), BigInt(1)), Seq(Epoch * IntervalMs * 10))
    control.eip37Calculate(w, Epoch) shouldBe BigInt(0)
    control.yoloEip37Calculate(w, Epoch) shouldBe BigInt(2)
    YoloDifficultyReference.eip37(w.map(blk), Epoch, IntervalMs, yoloChain.initialDifficulty) shouldBe BigInt(0)
    // and difficulty 1 cannot climb even after a very fast epoch: 1 * 3 / 2 = 1
    val fast = window(2 * Epoch, Seq(BigInt(1), BigInt(1)), Seq(Epoch.toLong))
    control.eip37Calculate(fast, Epoch) shouldBe BigInt(1)
  }

  property("floor: difficulty 2 climbs 2, 3, 4, 6, 9 under fast epochs") {
    val climb = Iterator.iterate(BigInt(2)) { p =>
      control.yoloEip37Calculate(window(2 * Epoch, Seq(p, p), Seq(Epoch.toLong)), Epoch)
    }.take(5).toList
    climb shouldBe List(2, 3, 4, 6, 9).map(BigInt(_))
  }

  property("floor: a result well above the floor is unchanged") {
    val p = normalized(BigInt("4500000000000")) // as a header carries it
    // on target: classic and predictive both give p
    val onTarget = window(1024, Seq.fill(9)(p), Seq.fill(8)(Epoch * IntervalMs))
    control.eip37Calculate(onTarget, Epoch) shouldBe normalized(p)
    control.yoloEip37Calculate(onTarget, Epoch) shouldBe control.eip37Calculate(onTarget, Epoch)
    // slow: falls to p / 2, still far above the floor
    val slow = window(1024, Seq.fill(9)(p), Seq.fill(8)(Epoch * IntervalMs * 10))
    control.eip37Calculate(slow, Epoch) shouldBe normalized(p / 2)
    control.yoloEip37Calculate(slow, Epoch) shouldBe control.eip37Calculate(slow, Epoch)
    // fast: climbs to 1.5 p
    val fast = window(1024, Seq.fill(9)(p), Seq.fill(8)(Epoch * IntervalMs / 10))
    control.eip37Calculate(fast, Epoch) shouldBe normalized(p * 3 / 2)
    control.yoloEip37Calculate(fast, Epoch) shouldBe control.eip37Calculate(fast, Epoch)
  }

  // --- random epochs -------------------------------------------------------------------------------------------

  /** Difficulty of the parent at a retarget: weighted towards the floor, plus mid-size and very large values. */
  private val parentDifficultyGen: Gen[BigInt] = Gen.frequency(
    5 -> Gen.choose(2L, 20L).map(BigInt(_)),
    2 -> Gen.choose(21L, 100000000L).map(BigInt(_)),
    3 -> (for {
      bits <- Gen.choose(24, 250)
      r <- Gen.choose(0L, Long.MaxValue)
    } yield BigInt(2).pow(bits) + r)
  ).map(normalized).suchThat(_ >= YoloSettings.MinDifficulty)

  /** Per-block time of an epoch, from 1 ms (hashrate spike) to 100x the target (hashrate gone). */
  private val epochMsGen: Gen[Long] = Gen.frequency(
    1 -> Gen.const(Epoch.toLong),
    3 -> Gen.choose(1L, IntervalMs).map(_ * Epoch),
    3 -> Gen.choose(IntervalMs, IntervalMs * 100).map(_ * Epoch),
    1 -> Gen.choose(IntervalMs / 2, IntervalMs * 2).map(_ * Epoch)
  )

  /** A retarget window: 2 to 9 epoch-end headers, last one at a YOLO retarget height, with difficulty P. */
  private val windowGen: Gen[Seq[Header]] = for {
    size <- Gen.choose(2, 9)
    p <- parentDifficultyGen
    // earlier epoch ends within a factor 8 of P either way
    factors <- Gen.listOfN(size - 1, Gen.choose(-3, 3))
    epochs <- Gen.listOfN(size - 1, epochMsGen)
    lastEpoch <- Gen.choose(size.max(2), 20000)
  } yield {
    val earlier = factors.map(f => normalized(if (f >= 0) p << f else (p >> -f).max(1)))
    window(lastEpoch * Epoch, earlier :+ p, epochs)
  }

  property("yoloEip37Calculate and eip37Calculate agree with the independent model over random epochs") {
    forAll(windowGen) { w =>
      val blks = w.map(blk)
      control.eip37Calculate(w, Epoch) shouldBe
        YoloDifficultyReference.eip37(blks, Epoch, IntervalMs, yoloChain.initialDifficulty)
      control.yoloEip37Calculate(w, Epoch) shouldBe
        YoloDifficultyReference.yoloRetarget(blks, Epoch, IntervalMs, yoloChain.initialDifficulty)
    }
  }

  property("relay bounds at a retarget: 2D <= 3P and D >= normalise(P / 2), with the floor, over random epochs") {
    var floorHits = 0
    forAll(windowGen) { w =>
      val p = w.last.requiredDifficulty
      val d = control.yoloEip37Calculate(w, Epoch)
      if (control.eip37Calculate(w, Epoch) < YoloSettings.MinDifficulty) floorHits += 1
      withClue(s"P = $p, D = $d: ") {
        (2 * d) should be <= (3 * p)
        d should be >= normalized(p / 2)
        d should be >= YoloSettings.MinDifficulty
        normalized(d) shouldBe d
      }
    }
    // the generator really exercises the floor
    floorHits should be > 0
  }

  property("relay bounds near the floor, exhaustively: P in 2..64, slow and fast epochs") {
    for {
      p <- (2 to 64).map(BigInt(_))
      perBlockMs <- Seq(1L, 1000L, IntervalMs / 2, IntervalMs, IntervalMs * 2, IntervalMs * 10, IntervalMs * 1000)
      size <- 2 to 9
    } {
      val w = window(size * Epoch, Seq.fill(size)(p), Seq.fill(size - 1)(perBlockMs * Epoch))
      val d = control.yoloEip37Calculate(w, Epoch)
      withClue(s"P = $p, per-block $perBlockMs ms, $size headers, D = $d: ") {
        (2 * d) should be <= (3 * p)
        d should be >= normalized(p / 2)
        d should be >= YoloSettings.MinDifficulty
      }
    }
  }

  property("the floor relies on P >= 2: at P = 1 (unreachable on YOLO) it would exceed the 1.5x climb bound") {
    // documents why the start-up check refuses an initial difficulty below the floor
    val w = window(2 * Epoch, Seq(BigInt(1), BigInt(1)), Seq(Epoch * IntervalMs))
    val d = control.yoloEip37Calculate(w, Epoch)
    d shouldBe BigInt(2)
    (2 * d) should be > (3 * BigInt(1))
  }

  property("independent nBits normalisation matches DifficultySerializer") {
    forAll(Gen.choose(0, 255), Gen.choose(0L, Long.MaxValue)) { (bits, r) =>
      val x = BigInt(2).pow(bits) + r
      YoloDifficultyReference.normalize(x) shouldBe normalized(x)
    }
    (0 to 100000).foreach(i => YoloDifficultyReference.normalize(BigInt(i)) shouldBe normalized(BigInt(i)))
  }

}
