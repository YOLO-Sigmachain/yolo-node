package org.ergoplatform.nodeView.history

import org.ergoplatform.consensus.ModifierSemanticValidity
import org.ergoplatform.mining.difficulty.DifficultySerializer
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.{Args, ChainSettings, ErgoSettingsReader, NetworkType, YoloSettings}
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.validation.RecoverableModifierError
import org.ergoplatform.wallet.utils.FileUtils
import scorex.util.encode.Base16

import scala.concurrent.duration._
import scala.util.{Failure, Try}

/**
  * Future-timestamp limit for headers (validation rule 211, `hdrFutureTimestamp`, in `HeadersProcessor`):
  * 4 block intervals with the YOLO switch on, 2 minutes at 30 s blocks; 10 block intervals, as upstream, with the
  * switch off. Headers are built with fake PoW and offered to a real headers-only history, as genesis and as a child,
  * so both validation paths are covered. A header beyond the limit must be refused as a recoverable error, so that
  * the node can accept it later, when its own clock has caught up.
  */
class YoloFutureTimestampSpec extends ErgoCorePropertyTest with FileUtils {
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.generators.ChainGenerator._

  private val Interval: FiniteDuration = 30.seconds
  private val GenesisDifficulty = BigInt(2)

  private val YoloLimitMs: Long = 2.minutes.toMillis
  private val ErgoLimitMs: Long = 5.minutes.toMillis

  /** Margin past a limit, large enough that a slow test run cannot bring the header back within it. */
  private val PastLimitMs: Long = Interval.toMillis

  private def chainSettingsFor(yoloOn: Boolean): ChainSettings = settings.chainSettings.copy(
    blockInterval = Interval,
    epochLength = YoloSettings.Eip37EpochLength,
    eip37EpochLength = Some(YoloSettings.Eip37EpochLength),
    useLastEpochs = 8,
    initialDifficultyHex = Base16.encode(GenesisDifficulty.toByteArray),
    yolo = if (yoloOn) YoloSettings(enabled = true) else YoloSettings.disabled)

  /** Headers-only history (digest state, no transaction checks), as in `YoloDifficultyHistorySpec`. */
  private def newHistory(yoloOn: Boolean): ErgoHistory = {
    val s = settings.copy(
      directory = createTempDir.getAbsolutePath,
      chainSettings = chainSettingsFor(yoloOn),
      nodeSettings = settings.nodeSettings.copy(stateType = StateType.Digest, verifyTransactions = false, blocksToKeep = 0))
    s.chainSettings.yolo.enabled shouldBe yoloOn
    ErgoHistory.readOrGenerate(s)(null)
  }

  private def mkHeader(history: ErgoHistory, parent: Option[Header], ts: Long): Header =
    nextHeader(parent, history.difficultyCalculator, tsOpt = Some(ts),
      diffBitsOpt = Some(DifficultySerializer.encodeCompactBits(GenesisDifficulty)), useRealTs = false)

  /** The rule's message, as upstream; the switch-on node adds its own limit to the detail. */
  private val RuleMessage = "Header timestamp should not be more than 20 minutes in the future. "

  private def shouldBeRefusedAsFuture(result: Try[Unit], yoloOn: Boolean): Unit = result match {
    case Failure(e: RecoverableModifierError) =>
      e.isFatal shouldBe false
      e.message should include("in the future")
      if (yoloOn) {
        // the detail gives this chain's limit, so the "20 minutes" of the rule's message is not the only number
        e.message should include(s", limit $YoloLimitMs ms")
        e.message should fullyMatch regex (java.util.regex.Pattern.quote(RuleMessage) + s"\\d+ vs \\d+, limit $YoloLimitMs ms")
      } else {
        // switch off: the message and its detail exactly as upstream, "<timestamp> vs <now>"
        e.message should not include ("limit")
        e.message should fullyMatch regex (java.util.regex.Pattern.quote(RuleMessage) + "\\d+ vs \\d+")
      }
    case other =>
      fail(s"expected a recoverable future-timestamp error, got $other")
  }

  property("the limit: 4 block intervals with the switch on, 10 with it off") {
    YoloSettings.MaxTimeDriftBlocks shouldBe 4
    chainSettingsFor(yoloOn = true).maxTimeDrift shouldBe YoloLimitMs
    chainSettingsFor(yoloOn = false).maxTimeDrift shouldBe ErgoLimitMs
    // upstream settings as they are: 10 block intervals
    settings.chainSettings.yolo.enabled shouldBe false
    settings.chainSettings.maxTimeDrift shouldBe 10 * settings.chainSettings.blockInterval.toMillis
  }

  property("switch on: a child header at now + 2 min is accepted, one past it is refused (recoverable)") {
    val history0 = newHistory(yoloOn = true)
    val now = System.currentTimeMillis()
    val genesis = mkHeader(history0, None, now - 1.hour.toMillis)
    val history = history0.append(genesis).get._1

    val tooFar = mkHeader(history, Some(genesis), System.currentTimeMillis() + YoloLimitMs + PastLimitMs)
    shouldBeRefusedAsFuture(history.applicableTry(tooFar), yoloOn = true)
    // refused recoverably: not stored and not marked invalid
    history.contains(tooFar.id) shouldBe false
    history.isSemanticallyValid(tooFar.id) should not be ModifierSemanticValidity.Invalid

    val atLimit = mkHeader(history, Some(genesis), System.currentTimeMillis() + YoloLimitMs)
    history.applicableTry(atLimit).isSuccess shouldBe true
    val after = history.append(atLimit).get._1
    after.bestHeaderOpt.map(_.id) shouldBe Some(atLimit.id)
  }

  property("switch on: a genesis header past now + 2 min is refused (recoverable), one at the limit is accepted") {
    val history = newHistory(yoloOn = true)
    val tooFar = mkHeader(history, None, System.currentTimeMillis() + YoloLimitMs + PastLimitMs)
    shouldBeRefusedAsFuture(history.applicableTry(tooFar), yoloOn = true)
    val atLimit = mkHeader(history, None, System.currentTimeMillis() + YoloLimitMs)
    history.applicableTry(atLimit).isSuccess shouldBe true
  }

  property("switch off: unchanged from upstream, a header past 2 min is accepted up to 10 block intervals (5 min)") {
    val history0 = newHistory(yoloOn = false)
    val now = System.currentTimeMillis()
    val genesis = mkHeader(history0, None, now - 1.hour.toMillis)
    val history = history0.append(genesis).get._1

    // what the switch-on node refuses, the switch-off node accepts
    val pastYoloLimit = mkHeader(history, Some(genesis), System.currentTimeMillis() + YoloLimitMs + PastLimitMs)
    history.applicableTry(pastYoloLimit).isSuccess shouldBe true
    val atErgoLimit = mkHeader(history, Some(genesis), System.currentTimeMillis() + ErgoLimitMs)
    history.applicableTry(atErgoLimit).isSuccess shouldBe true
    val tooFar = mkHeader(history, Some(genesis), System.currentTimeMillis() + ErgoLimitMs + PastLimitMs)
    shouldBeRefusedAsFuture(history.applicableTry(tooFar), yoloOn = false)

    // and the genesis path
    val fresh = newHistory(yoloOn = false)
    fresh.applicableTry(mkHeader(fresh, None, System.currentTimeMillis() + YoloLimitMs + PastLimitMs)).isSuccess shouldBe true
    shouldBeRefusedAsFuture(fresh.applicableTry(mkHeader(fresh, None, System.currentTimeMillis() + ErgoLimitMs + PastLimitMs)), yoloOn = false)
  }

  property("the YOLO settings files give the 2 minute limit") {
    val devnet = new java.io.File(getClass.getClassLoader.getResource("yolo-devnet.conf").toURI).getAbsolutePath
    // the full, checked read, as the node starts
    val s = ErgoSettingsReader.read(Args(Some(devnet), Some(NetworkType.TestNet)))
    s.chainSettings.blockInterval shouldBe 30.seconds
    s.chainSettings.maxTimeDrift shouldBe YoloLimitMs
  }

}
