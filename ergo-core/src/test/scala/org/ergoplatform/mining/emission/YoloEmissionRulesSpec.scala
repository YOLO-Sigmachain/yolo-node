package org.ergoplatform.mining.emission

import org.ergoplatform.utils.ErgoCorePropertyTest
import org.scalacheck.Gen

/**
  * YOLO emission curve and split, pinned to emission.es v1.4 and to
  * the reference model in the yolo-contracts repo (01-emission-tests/emission_model.py),
  * with vectors produced by running it.
  *
  * Two genesis values: the devnet genesis emission value, 177,291,250 YOLO (`DevnetGenesis`, the value yolo-devnet.conf
  * ships; terminal spend at 21,023,916 of a last 1.0 YOLO box), and the full schedule supply, 177,291,450 YOLO
  * (`Genesis`), kept ONLY as a cross-check of the rules against the schedule view (`start = total_supply()`) of
  * the reference model in the yolo-contracts repo (01-emission-tests/emission_model.py), which ends with a box of
  * exactly one 1.5 YOLO reward at 21,024,049.
  */
class YoloEmissionRulesSpec extends ErgoCorePropertyTest {
  import YoloEmissionRules._

  // cross-check only, not a launch value: total_supply() of
  // the reference model in the yolo-contracts repo (01-emission-tests/emission_model.py)
  private val Genesis = 177291450000000000L
  private val rules = new YoloEmissionRules(Genesis)
  // the devnet genesis emission value: genesis_box_value() of
  // the reference model in the yolo-contracts repo (01-emission-tests/emission_model.py)
  private val DevnetGenesis = 177291250000000000L
  private val devnetRules = new YoloEmissionRules(DevnetGenesis)
  private val B = BlocksPerHalving

  // cross-check against the reference model in the yolo-contracts repo (01-emission-tests/emission_model.py),
  // schedule view (177,291,450 YOLO): (height, reward, treasury, lp, miner, box value after the spend at that
  // height), from the model's block_reward, split_reward, and emission_box_value_at(h + 1, start = total_supply())
  // + block_reward(0) (the model counts height 0 as paid; the chain first spends the genesis box at height 1)
  private val vectors: Seq[(Int, Long, Long, Long, Long, Long)] = Seq(
    (0, 75000000000L, 3000000000L, 2250000000L, 69750000000L, 177291450000000000L),
    (1, 75000000000L, 3000000000L, 2250000000L, 69750000000L, 177291375000000000L),
    (2, 75000000000L, 3000000000L, 2250000000L, 69750000000L, 177291300000000000L),
    (1051199, 75000000000L, 3000000000L, 2250000000L, 69750000000L, 98451525000000000L),
    (1051200, 37500000000L, 1500000000L, 1125000000L, 34875000000L, 98451487500000000L),
    (1051201, 37500000000L, 1500000000L, 1125000000L, 34875000000L, 98451450000000000L),
    (2102399, 37500000000L, 1500000000L, 1125000000L, 34875000000L, 59031525000000000L),
    (2102400, 18750000000L, 750000000L, 562500000L, 17437500000L, 59031506250000000L),
    (2102401, 18750000000L, 750000000L, 562500000L, 17437500000L, 59031487500000000L),
    (3153599, 18750000000L, 750000000L, 562500000L, 17437500000L, 39321525000000000L),
    (3153600, 9375000000L, 375000000L, 281250000L, 8718750000L, 39321515625000000L),
    (3153601, 9375000000L, 375000000L, 281250000L, 8718750000L, 39321506250000000L),
    (4204799, 9375000000L, 375000000L, 281250000L, 8718750000L, 29466525000000000L),
    (4204800, 4687500000L, 187500000L, 140625000L, 4359375000L, 29466520312500000L),
    (4204801, 4687500000L, 187500000L, 140625000L, 4359375000L, 29466515625000000L),
    (5255999, 4687500000L, 187500000L, 140625000L, 4359375000L, 24539025000000000L),
    (5256000, 2343750000L, 93750000L, 70312500L, 2179687500L, 24539022656250000L),
    (5256001, 2343750000L, 93750000L, 70312500L, 2179687500L, 24539020312500000L),
    (6307199, 2343750000L, 93750000L, 70312500L, 2179687500L, 22075275000000000L),
    (6307200, 1500000000L, 60000000L, 45000000L, 1395000000L, 22075273500000000L),
    (6307201, 1500000000L, 60000000L, 45000000L, 1395000000L, 22075272000000000L),
    (7358400, 1500000000L, 60000000L, 45000000L, 1395000000L, 20498473500000000L)
  )

  property("constants match emission.es v1.4") {
    BlocksPerHalving shouldBe 1051200
    InitialReward shouldBe 75000000000L
    MinReward shouldBe 1500000000L
    FloorStartHeight shouldBe 6307200
    TreasuryPercent shouldBe 4L
    LpPercent shouldBe 3L
  }

  property("cross-check (177,291,450): reward, shares and remaining value match the reference model in the " +
    "yolo-contracts repo (01-emission-tests/emission_model.py) at height 1, every halving boundary and the floor") {
    vectors.foreach { case (h, reward, treasury, lp, miner, remaining) =>
      withClue(s"height $h: ") {
        rewardAtHeight(h) shouldBe reward
        sharesAtHeight(h) shouldBe YoloEmissionSplit(treasury, lp, miner)
        treasuryShareAtHeight(h) shouldBe treasury
        lpShareAtHeight(h) shouldBe lp
        minerShareAtHeight(h) shouldBe miner
        sharesAtHeight(h).total shouldBe reward
        rules.remainingAfter(h) shouldBe remaining
      }
    }
  }

  property("the six arms and the floor, at both sides of every boundary") {
    val arms = Seq(75000000000L, 37500000000L, 18750000000L, 9375000000L, 4687500000L, 2343750000L)
    arms.zipWithIndex.foreach { case (r, k) =>
      rewardAtHeight(k.toLong * B) shouldBe r
      rewardAtHeight((k + 1).toLong * B - 1) shouldBe r
    }
    // halvings = HEIGHT / blocksPerHalving: height 1,051,200 is already the second arm
    rewardAtHeight(1051199) shouldBe 75000000000L
    rewardAtHeight(1051200) shouldBe 37500000000L
    rewardAtHeight(6307199) shouldBe 2343750000L
    rewardAtHeight(6307200) shouldBe MinReward
    Seq(7L * B, 20L * B, 21023916L, Int.MaxValue.toLong).foreach(h => rewardAtHeight(h) shouldBe MinReward)
    // halvings <= 0 in the contract (height 0 is never spent, but the contract pays 75 there)
    rewardAtHeight(0) shouldBe InitialReward
    rewardAtHeight(-1) shouldBe InitialReward
  }

  property("normal-path split is integer arithmetic as emission.es: treasury r*4/100, LP r*3/100, miner the rest") {
    forAll(Gen.choose(0L, 100000000000L)) { r =>
      val s = split(r)
      s.treasury shouldBe r * 4 / 100
      s.lp shouldBe r * 3 / 100
      s.miner shouldBe r - r * 4 / 100 - r * 3 / 100
      s.total shouldBe r
    }
    // rounding goes to the miner
    split(99L) shouldBe YoloEmissionSplit(3L, 2L, 94L)
  }

  property("terminal split is taken on the box value") {
    terminalSplit(1500000000L) shouldBe YoloEmissionSplit(60000000L, 45000000L, 1395000000L)
    terminalSplit(1499999999L) shouldBe YoloEmissionSplit(59999999L, 44999999L, 1395000001L)
    terminalSplit(1L) shouldBe YoloEmissionSplit(0L, 0L, 1L)
    // at value == reward the terminal split equals the normal split
    Seq(1L, 1051200L, 6307200L, 21023916L).foreach { h =>
      terminalSplit(rewardAtHeight(h)) shouldBe sharesAtHeight(h)
    }
    // the interpreter throws on Long overflow; so does the split
    an[ArithmeticException] should be thrownBy split(Long.MaxValue / 3)
    // the first multiply to overflow is the treasury one (x 4): the exact edge
    split(Long.MaxValue / 4).total shouldBe Long.MaxValue / 4
    an[ArithmeticException] should be thrownBy split(Long.MaxValue / 4 + 1)
  }

  property("terminal path: SELF.value <= blockReward") {
    isTerminal(1500000000L, 21023916) shouldBe true
    isTerminal(1500000001L, 21023916) shouldBe false
    isTerminal(1499999999L, 21023916) shouldBe true
    isTerminal(75000000000L, 1) shouldBe true
    isTerminal(75000000001L, 1) shouldBe false
  }

  property("devnet genesis 177,291,250 YOLO: last normal spend at 21,023,915 from 2.5 YOLO, terminal spend of " +
    "1.0 YOLO at 21,023,916") {
    // the reference model in the yolo-contracts repo (01-emission-tests/emission_model.py) gives
    // real_terminal_height() = 21,023,916, real_box_value_at(21,023,916) = 1,000,000,000,
    // real_box_value_at(21,023,915) = 2,500,000,000
    devnetRules.terminalHeight shouldBe 21023916L
    devnetRules.isTerminalHeight(21023916L) shouldBe true
    devnetRules.boxValueAt(21023916L) shouldBe 1000000000L
    isTerminal(devnetRules.boxValueAt(21023916L), 21023916L) shouldBe true
    terminalSplit(1000000000L) shouldBe YoloEmissionSplit(40000000L, 30000000L, 930000000L)
    devnetRules.boxValueAt(21023915L) shouldBe 2500000000L
    isTerminal(devnetRules.boxValueAt(21023915L), 21023915L) shouldBe false
    devnetRules.remainingAfter(21023915L) shouldBe 1000000000L
    devnetRules.remainingAfter(21023916L) shouldBe 0L
    devnetRules.boxValueAt(21023917L) shouldBe 0L
    // real_box_value_at(h + 1) of the reference model in the yolo-contracts repo (01-emission-tests/emission_model.py)
    // at height 1, two halving boundaries and the floor start
    Seq(1L -> 177291175000000000L, 1051199L -> 98451325000000000L, 1051200L -> 98451287500000000L,
      2102399L -> 59031325000000000L, 2102400L -> 59031306250000000L, 6307200L -> 22075073500000000L).foreach {
      case (h, remaining) => withClue(s"height $h: ")(devnetRules.remainingAfter(h) shouldBe remaining)
    }
    // the devnet genesis is the schedule supply less the two 100 YOLO fund boxes: 133 fewer tail blocks
    Genesis - DevnetGenesis shouldBe 200L * CoinsInOneYolo
    rules.terminalHeight - devnetRules.terminalHeight shouldBe 133L
    paidThrough(21023915L) + 1000000000L shouldBe DevnetGenesis
  }

  property("cross-check (177,291,450): the box reaches value == reward == 1.5 YOLO at height 21,024,049") {
    rules.terminalHeight shouldBe 21024049L
    rules.isTerminalHeight(21024049L) shouldBe true
    rules.boxValueAt(21024049L) shouldBe 1500000000L
    rewardAtHeight(21024049L) shouldBe 1500000000L
    isTerminal(rules.boxValueAt(21024049L), 21024049L) shouldBe true
    rules.boxValueAt(21024048L) shouldBe 3000000000L
    isTerminal(rules.boxValueAt(21024048L), 21024048L) shouldBe false
    rules.remainingAfter(21024048L) shouldBe 1500000000L
    rules.remainingAfter(21024049L) shouldBe 0L
    rules.remainingAfter(Int.MaxValue.toLong) shouldBe 0L
    rules.boxValueAt(21024050L) shouldBe 0L
    // height 0 is never mined, so the box spent at height 21,024,000 still holds 75 YOLO (50 tail rewards):
    // 49 normal tail blocks, then the terminal one
    rules.boxValueAt(20L * B) shouldBe 75000000000L
    rules.remainingAfter(20L * B) shouldBe 73500000000L
    paidThrough(21024049L) shouldBe Genesis
  }

  property("a block-by-block walk from the devnet genesis agrees with the closed form, and pays exactly the genesis value") {
    var value = DevnetGenesis
    var paid = 0L
    var h = 0L
    var terminal = -1L
    val samples = Set(1L, 1051199L, 1051200L, 6307199L, 6307200L, 15000000L, 21023914L, 21023915L)
    while (terminal < 0) {
      h += 1
      val r = rewardAtHeight(h)
      if (isTerminal(value, h)) {
        val s = terminalSplit(value)
        s.total shouldBe value
        paid += value
        value = 0L
        terminal = h
      } else {
        val s = sharesAtHeight(h)
        s.total shouldBe r
        paid += r
        value -= r
      }
      if (samples.contains(h)) devnetRules.remainingAfter(h) shouldBe value
    }
    terminal shouldBe devnetRules.terminalHeight
    terminal shouldBe 21023916L
    paid shouldBe DevnetGenesis
    value shouldBe 0L
  }

  property("ARITHMETIC ONLY: terminal height and remaining value agree with a block walk for small genesis values") {
    // The rules' arithmetic on small values, not launch values: several of these (1 nano, 75 YOLO + 1 nano, ...)
    // end with a last emission box or terminal shares below the dust limit, outcomes consensus rejects, and the
    // launch check refuses such a genesis value (YoloGenesis.strandedEmissionError).
    // Small values end in the first arm; values around an arm boundary cross it.
    val values = Seq(1L, 74999999999L, 75000000000L, 75000000001L, 150000000000L, 150000000001L,
      75000000000L * 1051199, 75000000000L * 1051199 + 1, 75000000000L * 1051199 + 37500000000L,
      75000000000L * 1051199 + 37500000001L)
    values.foreach { g =>
      withClue(s"genesis $g: ") {
        val r = new YoloEmissionRules(g)
        // an independent block-by-block walk from the genesis value, checked against the closed form at every height
        var value = g
        var h = 0L
        var t = -1L
        while (t < 0) {
          h += 1
          if (isTerminal(value, h)) {
            t = h
          } else {
            if (r.boxValueAt(h) != value) fail(s"boxValueAt($h) = ${r.boxValueAt(h)}, the walk has $value")
            value -= rewardAtHeight(h)
            if (r.remainingAfter(h) != value) fail(s"remainingAfter($h) = ${r.remainingAfter(h)}, the walk has $value")
          }
          if (h > r.terminalHeight + 2) fail("walk passed the terminal height")
        }
        t shouldBe r.terminalHeight
        r.boxValueAt(t) shouldBe value
        value should be <= rewardAtHeight(t)
        r.remainingAfter(t) shouldBe 0L
      }
    }
    new YoloEmissionRules(1L).terminalHeight shouldBe 1L
    new YoloEmissionRules(75000000000L).terminalHeight shouldBe 1L
    new YoloEmissionRules(75000000001L).terminalHeight shouldBe 2L
    // 1,051,199 first-arm blocks, then the box holds exactly one 37.5 YOLO second-arm reward at height 1,051,200
    new YoloEmissionRules(75000000000L * 1051199 + 37500000000L).terminalHeight shouldBe 1051200L
    an[IllegalArgumentException] should be thrownBy new YoloEmissionRules(0L)
  }

}
