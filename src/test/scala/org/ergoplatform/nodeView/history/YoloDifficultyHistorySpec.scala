package org.ergoplatform.nodeView.history

import org.ergoplatform.mining.difficulty.{DifficultyAdjustment, DifficultySerializer, YoloDifficultyReference}
import org.ergoplatform.mining.difficulty.YoloDifficultyReference.Blk
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.history.ErgoHistoryUtils.Difficulty
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.{ChainSettings, ErgoSettings, YoloSettings}
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.wallet.utils.FileUtils
import scorex.util.encode.Base16

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._
import scala.util.Random

/**
  * YOLO difficulty rule in `HeadersProcessor.requiredDifficultyAfter`, which both
  * header validation (`validateChildBlockHeader`) and the candidate generator (`CandidateGenerator.createCandidate`)
  * call. Headers are built with fake PoW and appended to a real headers-only history, so every header goes through
  * validation, on the best-chain path and on the fork path.
  *
  * Switch on: every required difficulty is compared with the independent model `YoloDifficultyReference`.
  * Switch off: every required difficulty is compared with a verbatim copy of the v6.0.8 method (below).
  */
class YoloDifficultyHistorySpec extends ErgoCorePropertyTest with FileUtils {
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.generators.ChainGenerator._

  private val Epoch = YoloSettings.Eip37EpochLength
  private val IntervalMs: Long = 30.seconds.toMillis
  private val GenesisDifficulty = BigInt(2)

  /** Ten epochs and a bit, so the regression window fills (9 headers) and height 0 leaves it (parent 9 * Epoch). */
  private val ChainLength = 10 * Epoch + 3

  /** Mean per-block time of each epoch (index = (height - 1) / Epoch), +-50% jitter per block. */
  private val schedule: IndexedSeq[Long] = IndexedSeq(
    300000L, 300000L, // slow at genesis difficulty: the first retarget (parent 128) hits the floor
    1000L, 1000L, 1000L, 1000L, 100L, // hashrate arrives: climb, at most 1.5x per epoch
    30000L, // on target
    600000L, 600000L, // hashrate leaves: fall, at most 0.5x per epoch
    30000L)

  private def chainSettingsFor(yoloOn: Boolean): ChainSettings = settings.chainSettings.copy(
    blockInterval = 30.seconds,
    epochLength = Epoch,
    eip37EpochLength = Some(Epoch),
    useLastEpochs = 8,
    initialDifficultyHex = Base16.encode(GenesisDifficulty.toByteArray),
    yolo = if (yoloOn) YoloSettings(enabled = true) else YoloSettings.disabled)

  /** Headers-only history (digest state, no transaction checks), as `HistoryTestHelpers.generateHistory`. */
  private def newHistory(yoloOn: Boolean): (ErgoHistory, ErgoSettings) = {
    val s = settings.copy(
      directory = createTempDir.getAbsolutePath,
      chainSettings = chainSettingsFor(yoloOn),
      nodeSettings = settings.nodeSettings.copy(stateType = StateType.Digest, verifyTransactions = false, blocksToKeep = 0))
    s.chainSettings.initialDifficulty shouldBe GenesisDifficulty
    s.chainSettings.yolo.enabled shouldBe yoloOn
    (ErgoHistory.readOrGenerate(s)(null), s)
  }

  /** Block timestamps for heights 1..n (index i is height i + 1), starting 40 days ago. */
  private def timestamps(n: Int, seed: Long): IndexedSeq[Long] = {
    val rnd = new Random(seed)
    val start = System.currentTimeMillis() - 40.days.toMillis
    (2 to n).scanLeft(start) { (prev, height) =>
      val mean = schedule(((height - 1) / Epoch).min(schedule.size - 1))
      prev + math.max(1L, (mean * (0.5 + rnd.nextDouble())).toLong)
    }.toIndexedSeq
  }

  private def normalized(d: BigInt): BigInt =
    DifficultySerializer.decodeCompactBits(DifficultySerializer.encodeCompactBits(d))

  private def mkHeader(parent: Option[Header], ts: Long, difficulty: BigInt, control: DifficultyAdjustment): Header =
    nextHeader(parent, control, tsOpt = Some(ts),
      diffBitsOpt = Some(DifficultySerializer.encodeCompactBits(difficulty)), useRealTs = false)

  private def blk(h: Header): Blk = Blk(h.height, h.timestamp, h.requiredDifficulty)

  /** Required difficulty on YOLO from the independent model, for the chain ending at `chain.last`. */
  private def model(chain: IndexedSeq[Header]): BigInt =
    YoloDifficultyReference.requiredAfter(chain.map(blk), Epoch, IntervalMs, GenesisDifficulty)

  /**
    * `HeadersProcessor.requiredDifficultyAfter` of ergo v6.0.8 (commit c1a69f65d), copied verbatim except that
    * history members are reached through `history` and settings through `s`. The unmodified upstream code path.
    */
  private def upstreamRequiredDifficultyAfter(history: ErgoHistory, s: ErgoSettings, parent: Header): Difficulty = {
    val settings = s
    val chainSettings = s.chainSettings
    val difficultyCalculator = history.difficultyCalculator
    val historyReader = history

    val parentHeight = parent.height

    val eip37ActivationHeight = 844673

    if (settings.networkType.isMainNet && parentHeight + 1 >= eip37ActivationHeight) {
      val epochLength = 128 // epoch length after EIP-37 activation
      if (parentHeight % epochLength == 0) {
        val heights = difficultyCalculator.previousHeightsRequiredForRecalculation(parentHeight + 1, epochLength)
        val headers = if (historyReader.isInBestChain(parent)) {
          heights.flatMap(height => historyReader.bestHeaderAtHeight(height))
        } else {
          val chain = history.headerChainBack(heights.max - heights.min + 1, parent, _ => false)
          chain.headers.filter(p => heights.contains(p.height))
        }
        difficultyCalculator.eip37Calculate(headers, chainSettings.eip37EpochLength.get) // .get is ok for the mainnet
      } else {
        parent.requiredDifficulty
      }
    } else {
      if (parentHeight == settings.chainSettings.voting.version2ActivationHeight ||
          parent.height + 1 == settings.chainSettings.voting.version2ActivationHeight) {
        // Set difficulty for version 2 activation height (where specific difficulty is needed due to PoW change)
        settings.chainSettings.initialDifficultyVersion2
      } else {
        val epochLength = settings.chainSettings.epochLength

        if (parentHeight % epochLength == 0) {
          val heights = difficultyCalculator.previousHeightsRequiredForRecalculation(parentHeight + 1, epochLength)
            .ensuring(_.last == parentHeight)
          if (heights.lengthCompare(1) == 0) {
            difficultyCalculator.calculate(Array(parent), epochLength)
          } else {
            val headers = if (historyReader.isInBestChain(parent)) {
              heights.flatMap(height => historyReader.bestHeaderAtHeight(height))
            } else {
              val chain = history.headerChainBack(heights.max - heights.min + 1, parent, _ => false)
              chain.headers.filter(p => heights.contains(p.height))
            }
            difficultyCalculator.calculate(headers, epochLength)
          }
        } else {
          parent.requiredDifficulty
        }
      }
    }
  }

  /** Parent heights around the first epoch ends: the guard must hold at each (0 is the genesis block itself). */
  private val GuardParentHeights = Seq(63, 64, 65, 127, 128, 129, 192)

  /**
    * Builds a `ChainLength` chain on a fresh history. At each height the node's required difficulty must equal
    * `expected(chainSoFar, history, settings)`; the next header carries that difficulty and must be accepted, and a
    * header with a different difficulty must be refused.
    *
    * @return the chain, genesis first
    */
  private def buildChain(yoloOn: Boolean, seed: Long)
                        (expected: (IndexedSeq[Header], ErgoHistory, ErgoSettings) => BigInt): IndexedSeq[Header] = {
    val (h0, s) = newHistory(yoloOn)
    var history = h0
    val control = history.difficultyCalculator
    val ts = timestamps(ChainLength, seed)

    // parent height 0: the genesis block must carry the configured initial difficulty (HeadersProcessor rule 208)
    val badGenesis = mkHeader(None, ts.head, GenesisDifficulty + 1, control)
    history.applicableTry(badGenesis).isFailure shouldBe true
    val genesis = mkHeader(None, ts.head, GenesisDifficulty, control)
    history = history.append(genesis).get._1

    val chain = ArrayBuffer[Header](genesis)
    (2 to ChainLength).foreach { height =>
      val parent = chain.last
      val want = expected(chain.toIndexedSeq, history, s)
      withClue(s"switch ${if (yoloOn) "on" else "off"}, height $height: ") {
        history.requiredDifficultyAfter(parent) shouldBe want
        // the candidate generator sends it as nBits; validation compares the decoded nBits with it
        normalized(want) shouldBe want
        if (GuardParentHeights.contains(parent.height) || parent.height % Epoch == 0) {
          val wrong = normalized(want * 2)
          history.applicableTry(mkHeader(Some(parent), ts(height - 1), wrong, control)).isFailure shouldBe true
        }
      }
      val header = mkHeader(Some(parent), ts(height - 1), want, control)
      history = history.append(header).get._1
      history.bestHeaderOpt.map(_.id) shouldBe Some(header.id)
      chain += header
    }

    // with the whole chain in place, recomputing on the best-chain path gives the same values
    (1 until ChainLength).foreach { parentHeight =>
      history.requiredDifficultyAfter(chain(parentHeight - 1)) shouldBe chain(parentHeight).requiredDifficulty
    }
    chain.toIndexedSeq
  }

  property("switch on: required difficulty at every height of a 10-epoch chain equals the independent model") {
    var floorHits = 0
    var climbs = 0
    var falls = 0
    val chain = buildChain(yoloOn = true, seed = 1L) { (soFar, _, _) =>
      val want = model(soFar)
      val parent = soFar.last
      val p = parent.requiredDifficulty
      if (YoloDifficultyReference.isRetarget(parent.height, Epoch)) {
        val window = YoloDifficultyReference.windowHeights(parent.height, Epoch).map(h => blk(soFar(h - 1)))
        if (YoloDifficultyReference.eip37(window, Epoch, IntervalMs, GenesisDifficulty) < YoloDifficultyReference.Floor) {
          floorHits += 1
        }
        if (want > p) climbs += 1
        if (want < p) falls += 1
        // the two properties the header relay checks at a retarget
        (2 * want) should be <= (3 * p)
        want should be >= normalized(p / 2)
      } else {
        // and off a retarget
        want shouldBe p
      }
      want should be >= YoloSettings.MinDifficulty
      want
    }

    // block i + 1 is chain(i); parent height ph means the block at index ph
    def diffAfter(parentHeight: Int): BigInt = chain(parentHeight).requiredDifficulty
    def diffAt(height: Int): BigInt = chain(height - 1).requiredDifficulty

    // blocks 1 to 129 all at the genesis difficulty: no retarget at parent 64, and the first one (parent 128)
    // falls to 1 after the slow epochs and is floored back to 2
    (1 to 129).foreach(h => diffAt(h) shouldBe GenesisDifficulty)
    Seq(63, 64, 65, 127, 129).foreach(ph => diffAfter(ph) shouldBe diffAt(ph))
    // parent 192 retargets: climbs after the fast epoch
    diffAfter(192) should be > diffAt(192)
    diffAfter(192) shouldBe (diffAt(192) * 3 / 2)

    floorHits should be > 0
    climbs should be > 0
    falls should be > 0
  }

  property("switch off: same inputs, required difficulty at every height equals the unmodified v6.0.8 code") {
    var differsFromYolo = 0
    val chain = buildChain(yoloOn = false, seed = 1L) { (soFar, history, s) =>
      val want = upstreamRequiredDifficultyAfter(history, s, soFar.last)
      if (want != model(soFar)) differsFromYolo += 1
      want
    }
    // the classic regression keeps the parent's difficulty at parent 64 (one header), retargets from parent 128
    Seq(63, 64, 65, 127, 129).foreach(ph => chain(ph).requiredDifficulty shouldBe chain(ph - 1).requiredDifficulty)
    // and it is not the YOLO rule (uncapped, no floor), so the switch really changes something
    differsFromYolo should be > 0
  }

  property("block 129 retargets on both the best-chain and the fork path, switch on and off") {
    Seq(true, false).foreach { yoloOn =>
      withClue(s"switch ${if (yoloOn) "on" else "off"}: ") {
        val (h0, s) = newHistory(yoloOn)
        var history = h0
        val control = history.difficultyCalculator
        val start = System.currentTimeMillis() - 10.days.toMillis

        def expected(chain: IndexedSeq[Header]): BigInt =
          if (yoloOn) model(chain) else upstreamRequiredDifficultyAfter(history, s, chain.last)

        // A: 128 blocks on target; B: forks after height 100 and mines 101..128 in 1 ms each
        val a = ArrayBuffer(mkHeader(None, start, GenesisDifficulty, control))
        history = history.append(a.head).get._1
        (2 to 2 * Epoch).foreach { height =>
          val hdr = mkHeader(Some(a.last), a.last.timestamp + IntervalMs, expected(a.toIndexedSeq), control)
          history = history.append(hdr).get._1
          a += hdr
        }
        val b = ArrayBuffer(a.take(100): _*)
        (101 to 2 * Epoch).foreach { height =>
          val hdr = mkHeader(Some(b.last), b.last.timestamp + 1, expected(b.toIndexedSeq), control)
          history = history.append(hdr).get._1
          b += hdr
        }

        // equal scores: A stays the best chain, B's tip is on a fork
        history.bestHeaderOpt.map(_.id) shouldBe Some(a.last.id)
        history.isInBestChain(b.last) shouldBe false

        val wantA = expected(a.toIndexedSeq)
        val wantB = expected(b.toIndexedSeq)
        wantA shouldBe GenesisDifficulty
        wantB should be > wantA
        if (yoloOn) {
          wantB shouldBe GenesisDifficulty * 3 / 2
        } else {
          // v6.0.8 classic rule at parent 128: heights 0, 64, 128, height 0 does not exist, so the regression has
          // one point from headers 64 and 128 of A (all at difficulty 2, exactly 30 s apart):
          // 2 * 30000 ms * 64 / (64 * 30000 ms) = 2, and nBits represents 2 exactly
          wantA shouldBe BigInt(2)
        }

        // fork path (B), then best-chain path (A)
        history.requiredDifficultyAfter(b.last) shouldBe wantB
        history.requiredDifficultyAfter(a.last) shouldBe wantA
        history.applicableTry(mkHeader(Some(b.last), b.last.timestamp + 1, wantA, control)).isFailure shouldBe true
        val b129 = mkHeader(Some(b.last), b.last.timestamp + 1, wantB, control)
        history = history.append(b129).get._1
        history.bestHeaderOpt.map(_.id) shouldBe Some(b129.id)

        // now A's tip is on the fork
        history.isInBestChain(a.last) shouldBe false
        history.requiredDifficultyAfter(a.last) shouldBe wantA
        history.applicableTry(mkHeader(Some(a.last), a.last.timestamp + IntervalMs, wantB, control)).isFailure shouldBe true
        val a129 = mkHeader(Some(a.last), a.last.timestamp + IntervalMs, wantA, control)
        history = history.append(a129).get._1
        history.bestHeaderOpt.map(_.id) shouldBe Some(b129.id)
      }
    }
  }

}
