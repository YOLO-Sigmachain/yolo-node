package org.ergoplatform.http.api

import akka.actor.ActorRefFactory
import akka.http.scaladsl.server.Route
import io.circe.syntax._
import io.circe.{Encoder, Json}
import org.ergoplatform.mining.emission.{EmissionRules, YoloEmissionRules, YoloEmissionSplit}
import org.ergoplatform.settings.{ErgoSettings, RESTApiSettings, ReemissionSettings, YoloEmissionSettings}
import org.ergoplatform.{ErgoAddressEncoder, ErgoTreePredef, Pay2SAddress}
import scorex.core.api.http.ApiResponse

/**
  * `/emission` routes. With the YOLO switch off (`ergo.chain.yolo.enabled = false`) they report Ergo's emission
  * curve, EIP-27 re-emission and scripts as upstream. With it on they report the YOLO
  * emission curve and split (`ChainSettings.yoloEmissionRulesOpt`) and the configured YOLO emission and fund
  * scripts; EIP-27 is inert on YOLO, so the re-emission scripts are omitted and `reemitted` is 0.
  */
case class EmissionApiRoute(ergoSettings: ErgoSettings)
                           (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute {

  import EmissionApiRoute._

  override val settings: RESTApiSettings = ergoSettings.scorexSettings.restApi

  private val chainSettings = ergoSettings.chainSettings
  private val emissionRules = chainSettings.emissionRules

  private val reemissionSettings = chainSettings.reemission

  // None when the YOLO switch is off: no YOLO object is built then
  private val yoloRulesOpt: Option[YoloEmissionRules] = chainSettings.yoloEmissionRulesOpt

  override implicit val ergoAddressEncoder: ErgoAddressEncoder = new ErgoAddressEncoder(chainSettings.addressPrefix)

  override def route: Route = pathPrefix("emission") {
    emissionAt ~ scripts
  }

  /**
    * API method to display emission data for given height
    */
  val emissionAt: Route = (pathPrefix("at" / IntNumber) & get) { height =>
    yoloRulesOpt match {
      case Some(yoloRules) => ApiResponse(yoloEmissionInfoAtHeight(height, yoloRules))
      case None => ApiResponse(emissionInfoAtHeight(height, emissionRules, reemissionSettings))
    }
  }

  /**
    * API method which displays emission and re-emission related scripts as P2S addresses
    */
  val scripts: Route = pathPrefix("scripts") {
    if (yoloRulesOpt.isDefined) {
      ApiResponse(yoloScripts(chainSettings.yolo.emission))
    } else {
      val ms = ergoSettings.chainSettings.monetary

      ApiResponse(
        Json.obj(
          "emission" -> Pay2SAddress(ErgoTreePredef.emissionBoxProp(ms)).toString().asJson,
          "reemission" -> Pay2SAddress(reemissionSettings.reemissionRules.reemissionBoxProp(ms)).toString().asJson,
          "pay2Reemission" -> Pay2SAddress(reemissionSettings.reemissionRules.payToReemission).toString().asJson
        )
      )
    }
  }

}

object EmissionApiRoute {

  /**
    * Data container for emission/at API request output. Contains emission info for a block at a given height.
    *
    * @param height - height emission info is given for
    * @param minerReward - miner reward for given height
    * @param totalCoinsIssued - total amount of ERG emitted
    * @param totalRemainCoins - total amount of ERGs left in the emission contract box
    * @param reemissionAmt - re-emission tokens issuance for given height (if EIP-27 is activated)
    */
  final case class EmissionInfo(height: Int,
                                minerReward: Long,
                                totalCoinsIssued: Long,
                                totalRemainCoins: Long,
                                reemissionAmt: Long)

  def emissionInfoAtHeight(height: Int,
                           emissionRules: EmissionRules,
                           reemissionSettings: ReemissionSettings): EmissionInfo = {
    val reemissionAmt = reemissionSettings.reemissionRules.reemissionForHeight(height.toInt, emissionRules)
    val minerReward = emissionRules.minersRewardAtHeight(height) - reemissionAmt
    val totalCoinsIssued = emissionRules.issuedCoinsAfterHeight(height)
    val totalRemainCoins = emissionRules.coinsTotal - totalCoinsIssued
    EmissionInfo(height, minerReward, totalCoinsIssued, totalRemainCoins, reemissionAmt)
  }

  // todo: add totalReemitted ?
  implicit val encoder: Encoder[EmissionInfo] = (ei: EmissionInfo) => Json.obj(
    "height" -> Json.fromInt(ei.height),
    "minerReward" -> Json.fromLong(ei.minerReward),
    "totalCoinsIssued" -> Json.fromLong(ei.totalCoinsIssued),
    "totalRemainCoins" -> Json.fromLong(ei.totalRemainCoins),
    "reemitted" -> Json.fromLong(ei.reemissionAmt)
  )

  /**
    * emission/at output on a YOLO chain: the split of the emission spend at `height` and the emission box
    * balance once that spend is done.
    *
    * @param height           - height emission info is given for
    * @param split            - treasury, LP and miner parts of the emission spend at `height` (all 0 when there is
    *                           no emission spend at that height)
    * @param totalCoinsIssued - coins released from the emission box at heights 1 to `height`
    * @param totalRemainCoins - value left in the emission box after the spend at `height` (0 once it is gone)
    */
  final case class YoloEmissionInfo(height: Int,
                                    split: YoloEmissionSplit,
                                    totalCoinsIssued: Long,
                                    totalRemainCoins: Long) {
    def minerReward: Long = split.miner
    def treasuryReward: Long = split.treasury
    def lpReward: Long = split.lp
    def blockReward: Long = split.total
  }

  /**
    * Split of the YOLO emission spend at `height`, as the coinbase builder (`YoloEmissionTx.build`) takes it: no
    * spend below height 1; on the terminal path (box value at most the reward) the split of the box value; else the
    * split of the reward. After the terminal height the box value is 0, so the terminal branch gives all zeros.
    */
  def yoloSplitAtHeight(height: Int, rules: YoloEmissionRules): YoloEmissionSplit = {
    if (height < 1) {
      YoloEmissionSplit(0L, 0L, 0L)
    } else {
      val boxValue = rules.boxValueAt(height)
      if (YoloEmissionRules.isTerminal(boxValue, height)) {
        YoloEmissionRules.terminalSplit(boxValue)
      } else {
        YoloEmissionRules.split(YoloEmissionRules.rewardAtHeight(height))
      }
    }
  }

  def yoloEmissionInfoAtHeight(height: Int, rules: YoloEmissionRules): YoloEmissionInfo = {
    val totalRemainCoins = rules.remainingAfter(height)
    YoloEmissionInfo(height, yoloSplitAtHeight(height, rules), rules.genesisValue - totalRemainCoins, totalRemainCoins)
  }

  implicit val yoloEncoder: Encoder[YoloEmissionInfo] = (ei: YoloEmissionInfo) => Json.obj(
    "height" -> Json.fromInt(ei.height),
    "minerReward" -> Json.fromLong(ei.minerReward),
    "treasuryReward" -> Json.fromLong(ei.treasuryReward),
    "lpReward" -> Json.fromLong(ei.lpReward),
    "blockReward" -> Json.fromLong(ei.blockReward),
    "totalCoinsIssued" -> Json.fromLong(ei.totalCoinsIssued),
    "totalRemainCoins" -> Json.fromLong(ei.totalRemainCoins),
    "reemitted" -> Json.fromLong(0L) // EIP-27 is inert on YOLO
  )

  /**
    * emission/scripts output on a YOLO chain: the configured emission and fund contracts as P2S addresses.
    * The trees are parsed lazily from `ergo.chain.yolo.emission`; the start-up check refuses malformed ones.
    */
  def yoloScripts(e: YoloEmissionSettings)(implicit encoder: ErgoAddressEncoder): Json = Json.obj(
    "emission" -> Pay2SAddress(e.emissionTree).toString().asJson,
    "treasuryAccumulation" -> Pay2SAddress(e.treasuryAccumulationTree).toString().asJson,
    "lpAccumulation" -> Pay2SAddress(e.lpAccumulationTree).toString().asJson,
    "treasuryFund" -> Pay2SAddress(e.treasuryFundTree).toString().asJson,
    "lpFund" -> Pay2SAddress(e.lpFundTree).toString().asJson
  )

}
