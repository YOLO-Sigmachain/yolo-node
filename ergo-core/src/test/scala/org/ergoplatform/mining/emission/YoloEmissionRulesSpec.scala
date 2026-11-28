package org.ergoplatform.mining.emission

import org.ergoplatform.utils.ErgoCorePropertyTest
import org.scalacheck.Gen

/**
  * YOLO emission curve and split, pinned to emission.es v1.5 and to
  * the reference model in the yolo-contracts repo (01-emission-tests/emission_model.py),
  * with vectors produced by running it.
  *
  * Two genesis values: the devnet genesis emission value, 177,291,250 YOLO (`DevnetGenesis`, the value yolo-devnet.conf
  * ships; terminal spend at 26,258,941 of a last 1.5625 YOLO box), and the full schedule supply, 177,291,450 YOLO
  * (`Genesis`), kept ONLY as a cross-check of the rules against the schedule view (`start = total_supply()`) of
  * the reference model in the yolo-contracts repo (01-emission-tests/emission_model.py), which ends with a box of
  * exactly one 4.6875 YOLO reward at 26,258,983.
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
    (0, 37500000000L, 1500000000L, 1125000000L, 34875000000L, 177291450000000000L),
    (1, 37500000000L, 1500000000L, 1125000000L, 34875000000L, 177291412500000000L),
    (2, 37500000000L, 1500000000L, 1125000000L, 34875000000L, 177291375000000000L),
    (1051199, 37500000000L, 1500000000L, 1125000000L, 34875000000L, 137871487500000000L),
    (1051200, 18750000000L, 750000000L, 562500000L, 17437500000L, 137871468750000000L),
    (1051201, 18750000000L, 750000000L, 562500000L, 17437500000L, 137871450000000000L),
    (2102399, 18750000000L, 750000000L, 562500000L, 17437500000L, 118161487500000000L),
    (2102400, 9375000000L, 375000000L, 281250000L, 8718750000L, 118161478125000000L),
    (2102401, 9375000000L, 375000000L, 281250000L, 8718750000L, 118161468750000000L),
    (3153599, 9375000000L, 375000000L, 281250000L, 8718750000L, 108306487500000000L),
    (3153600, 4687500000L, 187500000L, 140625000L, 4359375000L, 108306482812500000L),
    (3153601, 4687500000L, 187500000L, 140625000L, 4359375000L, 108306478125000000L),
    (4204800, 4687500000L, 187500000L, 140625000L, 4359375000L, 103378982812500000L),
    (7358400, 4687500000L, 187500000L, 140625000L, 4359375000L, 88596482812500000L)
  )

  property("constants match emission.es v1.5") {
    BlocksPerHalving shouldBe 1051200
    InitialReward shouldBe 37500000000L
    MinReward shouldBe 4687500000L
    // the tail is the next halving held for good
    MinReward shouldBe InitialReward / 8
    ExplicitArms shouldBe 3
    FloorStartHeight shouldBe 3153600
    TreasuryPercent shouldBe 4L
    LpPercent shouldBe 3L
  }

  property("cross-check (177,291,450): reward, shares and remaining value match the reference model in the " +
    "yolo-contracts repo (01-emission-tests/emission_model.py) at height 1, every halving boundary and the tail") {
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

  property("the three arms and the tail, at both sides of every boundary") {
    val arms = Seq(37500000000L, 18750000000L, 9375000000L)
    arms.zipWithIndex.foreach { case (r, k) =>
      rewardAtHeight(k.toLong * B) shouldBe r
      rewardAtHeight((k + 1).toLong * B - 1) shouldBe r
    }
    // halvings = HEIGHT / blocksPerHalving: height 1,051,200 is already the second arm
    rewardAtHeight(1) shouldBe 37500000000L
    rewardAtHeight(1051199) shouldBe 37500000000L
    rewardAtHeight(1051200) shouldBe 18750000000L
    rewardAtHeight(2102399) shouldBe 18750000000L
    rewardAtHeight(2102400) shouldBe 9375000000L
    rewardAtHeight(3153599) shouldBe 9375000000L
    rewardAtHeight(3153600) shouldBe MinReward
    Seq(4L * B, 7L * B, 20L * B, 26258940L, 26258941L, 26258942L, Int.MaxValue.toLong).foreach(h =>
      rewardAtHeight(h) shouldBe MinReward)
    // halvings <= 0 in the contract (height 0 is never spent, but the contract pays 37.5 there)
    rewardAtHeight(0) shouldBe InitialReward
    rewardAtHeight(-1) shouldBe InitialReward
  }

  property("shares at heights 1, 1,051,199 / 1,051,200, 2,102,399 / 2,102,400, 3,153,599 / 3,153,600 and " +
    "26,258,940 to 26,258,942: exact nanocoins, miner the residual") {
    Seq(
      (1L, 1500000000L, 1125000000L, 34875000000L),
      (1051199L, 1500000000L, 1125000000L, 34875000000L),
      (1051200L, 750000000L, 562500000L, 17437500000L),
      (2102399L, 750000000L, 562500000L, 17437500000L),
      (2102400L, 375000000L, 281250000L, 8718750000L),
      (3153599L, 375000000L, 281250000L, 8718750000L),
      (3153600L, 187500000L, 140625000L, 4359375000L),
      (26258940L, 187500000L, 140625000L, 4359375000L),
      (26258941L, 187500000L, 140625000L, 4359375000L),
      (26258942L, 187500000L, 140625000L, 4359375000L)
    ).foreach { case (h, treasury, lp, miner) =>
      withClue(s"height $h: ") {
        sharesAtHeight(h) shouldBe YoloEmissionSplit(treasury, lp, miner)
        sharesAtHeight(h).total shouldBe rewardAtHeight(h)
        // every share is an exact number of nanocoins at each reward level: nothing is rounded away
        treasury * 100 shouldBe rewardAtHeight(h) * 4
        lp * 100 shouldBe rewardAtHeight(h) * 3
      }
    }
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
    terminalSplit(4687500000L) shouldBe YoloEmissionSplit(187500000L, 140625000L, 4359375000L)
    terminalSplit(4687499999L) shouldBe YoloEmissionSplit(187499999L, 140624999L, 4359375001L)
    terminalSplit(1L) shouldBe YoloEmissionSplit(0L, 0L, 1L)
    // at value == reward the terminal split equals the normal split
    Seq(1L, 1051200L, 2102400L, 3153600L, 26258941L).foreach { h =>
      terminalSplit(rewardAtHeight(h)) shouldBe sharesAtHeight(h)
    }
    // the interpreter throws on Long overflow; so does the split
    an[ArithmeticException] should be thrownBy split(Long.MaxValue / 3)
    // the first multiply to overflow is the treasury one (x 4): the exact edge
    split(Long.MaxValue / 4).total shouldBe Long.MaxValue / 4
    an[ArithmeticException] should be thrownBy split(Long.MaxValue / 4 + 1)
  }

  property("terminal path: SELF.value <= blockReward") {
    isTerminal(4687500000L, 26258941) shouldBe true
    isTerminal(4687500001L, 26258941) shouldBe false
    isTerminal(4687499999L, 26258941) shouldBe true
    isTerminal(37500000000L, 1) shouldBe true
    isTerminal(37500000001L, 1) shouldBe false
  }

  property("devnet genesis 177,291,250 YOLO: last normal spend at 26,258,940 from 6.25 YOLO, terminal spend of " +
    "1.5625 YOLO at 26,258,941") {
    // the reference model in the yolo-contracts repo (01-emission-tests/emission_model.py) gives
    // real_terminal_height() = 26,258,941, real_box_value_at(26,258,941) = 1,562,500,000,
    // real_box_value_at(26,258,940) = 6,250,000,000
    devnetRules.terminalHeight shouldBe 26258941L
    devnetRules.isTerminalHeight(26258941L) shouldBe true
    devnetRules.isTerminalHeight(26258940L) shouldBe false
    devnetRules.isTerminalHeight(26258942L) shouldBe false
    devnetRules.boxValueAt(26258941L) shouldBe 1562500000L
    isTerminal(devnetRules.boxValueAt(26258941L), 26258941L) shouldBe true
    terminalSplit(1562500000L) shouldBe YoloEmissionSplit(62500000L, 46875000L, 1453125000L)
    devnetRules.boxValueAt(26258940L) shouldBe 6250000000L
    isTerminal(devnetRules.boxValueAt(26258940L), 26258940L) shouldBe false
    devnetRules.remainingAfter(26258940L) shouldBe 1562500000L
    devnetRules.remainingAfter(26258941L) shouldBe 0L
    devnetRules.remainingAfter(26258942L) shouldBe 0L
    devnetRules.boxValueAt(26258942L) shouldBe 0L
    // real_box_value_at(h + 1) of the reference model in the yolo-contracts repo (01-emission-tests/emission_model.py)
    // at height 1, the two halving boundaries and the tail start
    Seq(1L -> 177291212500000000L, 1051199L -> 137871287500000000L, 1051200L -> 137871268750000000L,
      2102399L -> 118161287500000000L, 2102400L -> 118161278125000000L, 3153599L -> 108306287500000000L,
      3153600L -> 108306282812500000L).foreach {
      case (h, remaining) => withClue(s"height $h: ")(devnetRules.remainingAfter(h) shouldBe remaining)
    }
    // the box spent at each of those heights: the value before the spend
    Seq(1L -> 177291250000000000L, 1051199L -> 137871325000000000L, 1051200L -> 137871287500000000L,
      2102399L -> 118161306250000000L, 2102400L -> 118161287500000000L, 3153599L -> 108306296875000000L,
      3153600L -> 108306287500000000L).foreach {
      case (h, value) => withClue(s"height $h: ")(devnetRules.boxValueAt(h) shouldBe value)
    }
    // the devnet genesis is the schedule supply less the two 100 YOLO fund boxes: 42 fewer tail blocks
    Genesis - DevnetGenesis shouldBe 200L * CoinsInOneYolo
    rules.terminalHeight - devnetRules.terminalHeight shouldBe 42L
    paidThrough(26258940L) + 1562500000L shouldBe DevnetGenesis
  }

  property("devnet genesis: lifetime treasury 7,091,650, LP 5,318,737.5 and miners 164,880,862.5 YOLO") {
    // normal spends at heights 1 to 26,258,940, then the terminal split of the last 1.5625 YOLO
    val t = devnetRules.terminalHeight
    val arms = (0 until ExplicitArms).map { k =>
      val start = if (k == 0) 1L else k.toLong * B
      (sharesAtHeight(start), (k + 1).toLong * B - start)
    } :+ ((sharesAtHeight(FloorStartHeight), t - FloorStartHeight))
    val last = terminalSplit(devnetRules.boxValueAt(t))
    val treasury = arms.map { case (sh, n) => sh.treasury * n }.sum + last.treasury
    val lp = arms.map { case (sh, n) => sh.lp * n }.sum + last.lp
    val miner = arms.map { case (sh, n) => sh.miner * n }.sum + last.miner
    arms.map(_._2).sum shouldBe t - 1
    treasury shouldBe 7091650000000000L
    lp shouldBe 5318737500000000L
    miner shouldBe 164880862500000000L
    treasury + lp + miner shouldBe DevnetGenesis
  }

  property("cross-check (177,291,450): the box reaches value == reward == 4.6875 YOLO at height 26,258,983") {
    rules.terminalHeight shouldBe 26258983L
    rules.isTerminalHeight(26258983L) shouldBe true
    rules.boxValueAt(26258983L) shouldBe 4687500000L
    rewardAtHeight(26258983L) shouldBe 4687500000L
    isTerminal(rules.boxValueAt(26258983L), 26258983L) shouldBe true
    rules.boxValueAt(26258982L) shouldBe 9375000000L
    isTerminal(rules.boxValueAt(26258982L), 26258982L) shouldBe false
    rules.remainingAfter(26258982L) shouldBe 4687500000L
    rules.remainingAfter(26258983L) shouldBe 0L
    rules.remainingAfter(Int.MaxValue.toLong) shouldBe 0L
    rules.boxValueAt(26258984L) shouldBe 0L
    // height 0 is never mined, so the box spent at height 26,258,976 (the model's SCHEDULE_END_HEIGHT, where the
    // schedule view paid from height 0 holds 0) still holds 37.5 YOLO (8 tail rewards): 7 normal tail blocks, then
    // the terminal one
    rules.boxValueAt(26258976L) shouldBe 37500000000L
    rules.remainingAfter(26258976L) shouldBe 32812500000L
    paidThrough(26258983L) shouldBe Genesis
  }

  property("Long range: the terminal height stays an Int height for any genesis value; paidThrough's overflow edge") {
    val max = new YoloEmissionRules(Long.MaxValue)
    max.terminalHeight shouldBe 1956089509L
    max.terminalHeight should be < Int.MaxValue.toLong
    max.boxValueAt(1956089509L) shouldBe 917275807L
    isTerminal(max.boxValueAt(1956089509L), 1956089509L) shouldBe true
    max.boxValueAt(1956089508L) shouldBe 5604775807L
    // the last height whose sum fits a Long, then the interpreter-style overflow
    paidThrough(1956089508L) shouldBe 9223372035937500000L
    an[ArithmeticException] should be thrownBy paidThrough(1956089509L)
  }

  property("a block-by-block walk from the devnet genesis agrees with the closed form, and pays exactly the genesis value") {
    var value = DevnetGenesis
    var paid = 0L
    var h = 0L
    var terminal = -1L
    val samples = Set(1L, 1051199L, 1051200L, 2102399L, 2102400L, 3153599L, 3153600L, 15000000L, 26258939L,
      26258940L)
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
    terminal shouldBe 26258941L
    paid shouldBe DevnetGenesis
    value shouldBe 0L
  }

  property("ARITHMETIC ONLY: terminal height and remaining value agree with a block walk for small genesis values") {
    // The rules' arithmetic on small values, not launch values: several of these (1 nano, 37.5 YOLO + 1 nano, ...)
    // end with a last emission box or terminal shares below the dust limit, outcomes consensus rejects, and the
    // launch check refuses such a genesis value (YoloGenesis.strandedEmissionError).
    // Small values end in the first arm; values around an arm boundary cross it.
    val values = Seq(1L, 37499999999L, 37500000000L, 37500000001L, 75000000000L, 75000000001L,
      37500000000L * 1051199, 37500000000L * 1051199 + 1, 37500000000L * 1051199 + 18750000000L,
      37500000000L * 1051199 + 18750000001L)
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
    new YoloEmissionRules(37500000000L).terminalHeight shouldBe 1L
    new YoloEmissionRules(37500000001L).terminalHeight shouldBe 2L
    new YoloEmissionRules(75000000000L).terminalHeight shouldBe 2L
    new YoloEmissionRules(75000000001L).terminalHeight shouldBe 3L
    // 1,051,199 first-arm blocks, then the box holds exactly one 18.75 YOLO second-arm reward at height 1,051,200
    new YoloEmissionRules(37500000000L * 1051199 + 18750000000L).terminalHeight shouldBe 1051200L
    new YoloEmissionRules(37500000000L * 1051199 + 18750000001L).terminalHeight shouldBe 1051201L
    an[IllegalArgumentException] should be thrownBy new YoloEmissionRules(0L)
  }

}
