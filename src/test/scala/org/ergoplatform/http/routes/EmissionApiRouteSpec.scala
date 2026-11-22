package org.ergoplatform.http.routes

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import akka.testkit.TestDuration
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.Json
import io.circe.syntax._
import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.http.api.EmissionApiRoute
import org.ergoplatform.mining.emission.{EmissionRules, YoloEmissionRules, YoloEmissionSplit}
import org.ergoplatform.settings.{Args, ErgoSettings, ErgoSettingsReader, NetworkType, ReemissionSettings,
  YoloEmissionSettings}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sigma.ast.ErgoTree

import java.io.File
import scala.concurrent.duration._

class EmissionApiRouteSpec extends AnyFlatSpec
  with Matchers
  with ScalatestRouteTest
  with FailFastCirceSupport {

  val prefix = "/emission/at"

  implicit val timeout: RouteTestTimeout = RouteTestTimeout(15.seconds.dilated)

  val ergoSettings: ErgoSettings = ErgoSettingsReader.read()
  val coinEmission: EmissionRules = ergoSettings.chainSettings.emissionRules

  val reemissionSettings: ReemissionSettings = ergoSettings.chainSettings.reemission

  val route: Route = EmissionApiRoute(ergoSettings).route

  it should "get correct emission values" in {
    Get(prefix + "/1") ~> route ~> check {
      status shouldBe StatusCodes.OK
      EmissionApiRoute.emissionInfoAtHeight(1, coinEmission, reemissionSettings).asJson shouldEqual responseAs[Json]
    }

    Get(prefix + "/10000") ~> route ~> check {
      status shouldBe StatusCodes.OK
      EmissionApiRoute.emissionInfoAtHeight(10000, coinEmission, reemissionSettings).asJson shouldEqual responseAs[Json]
    }

    Get(prefix + "/1000000") ~> route ~> check {
      status shouldBe StatusCodes.OK
      EmissionApiRoute.emissionInfoAtHeight(1000000, coinEmission, reemissionSettings).asJson shouldEqual responseAs[Json]
    }
  }

  // ---- YOLO switch off: the upstream response shape, no YOLO keys ----

  private val ErgoAtKeys = Set("height", "minerReward", "totalCoinsIssued", "totalRemainCoins", "reemitted")
  private val ErgoScriptKeys = Set("emission", "reemission", "pay2Reemission")

  private def keysOf(json: Json): Set[String] = json.asObject.get.keys.toSet

  it should "with the YOLO switch off, return exactly the upstream keys from /emission/at and /emission/scripts" in {
    ergoSettings.chainSettings.yolo.enabled shouldBe false
    ergoSettings.chainSettings.yoloEmissionRulesOpt shouldBe None
    Seq(0, 1, 1000000, Int.MaxValue).foreach { h =>
      Get(s"$prefix/$h") ~> route ~> check {
        status shouldBe StatusCodes.OK
        keysOf(responseAs[Json]) shouldBe ErgoAtKeys
      }
    }
    Get("/emission/scripts") ~> route ~> check {
      status shouldBe StatusCodes.OK
      keysOf(responseAs[Json]) shouldBe ErgoScriptKeys
    }
  }

  // ---- YOLO switch on ----

  // the YOLO devnet settings, read as the other YOLO specs read them (full, checked read)
  private lazy val devnetPath: String =
    new File(getClass.getClassLoader.getResource("yolo-devnet.conf").toURI).getAbsolutePath
  private lazy val devnet: ErgoSettings = ErgoSettingsReader.read(Args(Some(devnetPath), Some(NetworkType.TestNet)))
  private lazy val e: YoloEmissionSettings = devnet.chainSettings.yolo.emission
  private lazy val rules: YoloEmissionRules = devnet.chainSettings.yoloEmissionRulesOpt.get
  private lazy val yoloRoute: Route = EmissionApiRoute(devnet).route

  private def withGenesisValue(v: Long): ErgoSettings = devnet.copy(chainSettings = devnet.chainSettings.copy(
    yolo = devnet.chainSettings.yolo.copy(emission = e.copy(genesisValue = v))))

  private val YoloAtKeys = Set("height", "minerReward", "treasuryReward", "lpReward", "blockReward",
    "totalCoinsIssued", "totalRemainCoins", "reemitted")
  private val YoloScriptKeys = Set("emission", "treasuryAccumulation", "lpAccumulation", "treasuryFund", "lpFund")

  private val Zero = YoloEmissionSplit(0L, 0L, 0L)

  // expected (split, remaining) at h, from the YoloEmissionRules functions case by case (not via the route helper)
  private def expected(r: YoloEmissionRules, h: Int): (YoloEmissionSplit, Long) = {
    val t = r.terminalHeight
    if (h < 1) {
      (Zero, r.genesisValue)
    } else if (h < t) {
      (YoloEmissionRules.sharesAtHeight(h), r.genesisValue - YoloEmissionRules.paidThrough(h))
    } else if (h == t) {
      (YoloEmissionRules.terminalSplit(r.boxValueAt(h)), 0L)
    } else {
      (Zero, 0L)
    }
  }

  private def long(json: Json, key: String): Long = json.hcursor.downField(key).as[Long].toOption.get

  private def checkYoloAt(r: YoloEmissionRules, json: Json, h: Int): Unit = withClue(s"height $h: ") {
    keysOf(json) shouldBe YoloAtKeys
    val (split, remaining) = expected(r, h)
    json.hcursor.downField("height").as[Int].toOption.get shouldBe h
    long(json, "minerReward") shouldBe split.miner
    long(json, "treasuryReward") shouldBe split.treasury
    long(json, "lpReward") shouldBe split.lp
    long(json, "blockReward") shouldBe split.total
    long(json, "totalRemainCoins") shouldBe remaining
    long(json, "totalCoinsIssued") shouldBe r.genesisValue - remaining
    long(json, "reemitted") shouldBe 0L
    // the two invariants
    long(json, "totalCoinsIssued") + long(json, "totalRemainCoins") shouldBe r.genesisValue
    long(json, "minerReward") + long(json, "treasuryReward") + long(json, "lpReward") shouldBe
      long(json, "blockReward")
    // the box value after h is the box value spent at h + 1, and the spend at h released exactly blockReward
    if (h >= 0 && h < Int.MaxValue) long(json, "totalRemainCoins") shouldBe r.boxValueAt(h + 1L)
    if (h >= 1) r.boxValueAt(h) - long(json, "totalRemainCoins") shouldBe long(json, "blockReward")
  }

  it should "with the YOLO switch on, report the YOLO curve and split at the boundary heights" in {
    devnet.chainSettings.yolo.enabled shouldBe true
    val t = rules.terminalHeight
    t should be < Int.MaxValue.toLong
    val terminal = t.toInt
    // first tail (floor) height, derived from the rules
    val floor = YoloEmissionRules.FloorStartHeight
    YoloEmissionRules.rewardAtHeight(floor) shouldBe YoloEmissionRules.MinReward
    YoloEmissionRules.rewardAtHeight(floor - 1) should be > YoloEmissionRules.MinReward
    // the arm boundary: 1,051,200 is already in the second arm (halvings = height / BlocksPerHalving)
    val b = YoloEmissionRules.BlocksPerHalving
    YoloEmissionRules.rewardAtHeight(b - 1) shouldBe YoloEmissionRules.InitialReward
    YoloEmissionRules.rewardAtHeight(b) shouldBe YoloEmissionRules.InitialReward / 2
    // the terminal spend pays less than or exactly a full reward, the spend before it a full reward
    rules.boxValueAt(t) should be <= YoloEmissionRules.rewardAtHeight(t)
    rules.boxValueAt(t - 1) should be > YoloEmissionRules.rewardAtHeight(t - 1)

    val heights = Seq(0, 1, 2, b - 1, b, b + 1, floor - 1, floor, floor + 1, terminal - 1, terminal, terminal + 1,
      Int.MaxValue)
    heights.foreach { h =>
      Get(s"$prefix/$h") ~> yoloRoute ~> check {
        status shouldBe StatusCodes.OK
        val json = responseAs[Json]
        checkYoloAt(rules, json, h)
        if (h == 1 || h == terminal) info(s"YOLO devnet /emission/at/$h = ${json.noSpaces}")
      }
    }

    // spot values: 75 YOLO split 3.0 / 2.25 / 69.75 at height 1; 37.5 YOLO at the second arm
    Get(s"$prefix/1") ~> yoloRoute ~> check {
      val json = responseAs[Json]
      long(json, "minerReward") shouldBe 69750000000L
      long(json, "treasuryReward") shouldBe 3000000000L
      long(json, "lpReward") shouldBe 2250000000L
      long(json, "blockReward") shouldBe 75000000000L
      long(json, "totalCoinsIssued") shouldBe 75000000000L
    }
    Get(s"$prefix/$b") ~> yoloRoute ~> check {
      long(responseAs[Json], "blockReward") shouldBe 37500000000L
    }
    // terminal: the whole remaining box goes out, everything is issued; after it all zeros
    Get(s"$prefix/$terminal") ~> yoloRoute ~> check {
      val json = responseAs[Json]
      long(json, "blockReward") shouldBe rules.boxValueAt(t)
      long(json, "totalCoinsIssued") shouldBe rules.genesisValue
      long(json, "totalRemainCoins") shouldBe 0L
    }
    Get(s"$prefix/${terminal + 1}") ~> yoloRoute ~> check {
      val json = responseAs[Json]
      long(json, "blockReward") shouldBe 0L
      long(json, "totalCoinsIssued") shouldBe rules.genesisValue
    }
  }

  it should "with the YOLO switch on, not throw at negative and extreme heights" in {
    Seq(Int.MinValue, -1, 0, Int.MaxValue).foreach { h =>
      checkYoloAt(rules, EmissionApiRoute.yoloEmissionInfoAtHeight(h, rules).asJson, h)
    }
  }

  it should "with the YOLO switch on, take the terminal split on the box value (small genesis value)" in {
    // 235 YOLO: heights 1 to 3 pay 75 each, height 4 spends the last 10 YOLO, then nothing
    val coin = YoloEmissionRules.CoinsInOneYolo
    val s = withGenesisValue(235L * coin)
    val r = s.chainSettings.yoloEmissionRulesOpt.get
    r.terminalHeight shouldBe 4L
    val smallRoute = EmissionApiRoute(s).route
    (0 to 6).foreach { h =>
      Get(s"$prefix/$h") ~> smallRoute ~> check {
        status shouldBe StatusCodes.OK
        checkYoloAt(r, responseAs[Json], h)
      }
    }
    Get(s"$prefix/4") ~> smallRoute ~> check {
      val json = responseAs[Json]
      long(json, "treasuryReward") shouldBe 4L * coin / 10
      long(json, "lpReward") shouldBe 3L * coin / 10
      long(json, "minerReward") shouldBe 93L * coin / 10
      long(json, "blockReward") shouldBe 10L * coin
      long(json, "totalRemainCoins") shouldBe 0L
    }
  }

  it should "with the YOLO switch on, return the five YOLO scripts and no re-emission scripts" in {
    val encoder = new ErgoAddressEncoder(devnet.chainSettings.addressPrefix)
    Get("/emission/scripts") ~> yoloRoute ~> check {
      status shouldBe StatusCodes.OK
      val json = responseAs[Json]
      keysOf(json) shouldBe YoloScriptKeys
      json.hcursor.downField("reemission").succeeded shouldBe false
      json.hcursor.downField("pay2Reemission").succeeded shouldBe false
      val trees: Map[String, ErgoTree] = Map(
        "emission" -> e.emissionTree,
        "treasuryAccumulation" -> e.treasuryAccumulationTree,
        "lpAccumulation" -> e.lpAccumulationTree,
        "treasuryFund" -> e.treasuryFundTree,
        "lpFund" -> e.lpFundTree)
      trees.foreach { case (key, tree) =>
        withClue(s"$key: ") {
          val address = encoder.fromString(json.hcursor.downField(key).as[String].toOption.get).get
          address.script.bytes shouldEqual tree.bytes
        }
      }
      trees.values.map(_.bytes.toSeq).toSet.size shouldBe 5
    }
  }

}
