package org.ergoplatform.settings

import com.typesafe.config.ConfigFactory
import org.ergoplatform.mining.{AutolykosPowScheme, DefaultFakePowScheme}
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.utils.ErgoCorePropertyTest

import java.io.File
import java.net.InetSocketAddress
import scala.collection.JavaConverters._
import scala.concurrent.duration._

/**
  * The YOLO conf files (network magic, addresses, node name and peers, block interval, headerChainDiff,
  * rejectStorageRentTxs, EIP-27 off, powHeightDivisor and the switch) and the launch parameter selection behind
  * the switch.
  */
class YoloConfigSpec extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoNodeTestConstants._

  private def resourcePath(name: String): String =
    new File(getClass.getClassLoader.getResource(name).toURI).getAbsolutePath

  private lazy val yoloTestnetPath = resourcePath("yolo-testnet.conf")
  private lazy val yoloDevnetPath = resourcePath("yolo-devnet.conf")

  private def checkYoloTestnet(s: ErgoSettings): Unit = {
    s.networkType shouldBe NetworkType.TestNet

    // YOLO values
    s.scorexSettings.network.magicBytes.toSeq shouldBe "YOLO".getBytes("US-ASCII").toSeq
    s.scorexSettings.network.magicBytes.toSeq shouldBe Seq[Byte](89, 79, 76, 79)
    s.scorexSettings.network.bindAddress shouldBe new InetSocketAddress("0.0.0.0", 9120)
    // REST API on the local interface only (the API key hash is the upstream default)
    s.scorexSettings.restApi.bindAddress shouldBe new InetSocketAddress("127.0.0.1", 9153)
    // own data directory, never the Ergo ".ergo" one (DATADIR overrides it, as upstream)
    s.directory shouldBe sys.env.getOrElse("DATADIR", System.getProperty("user.dir") + "/.yolo")
    s.directory should not endWith "/.ergo"
    s.scorexSettings.network.knownPeers shouldBe Seq.empty
    s.scorexSettings.network.nodeName should startWith("yolo-")
    s.chainSettings.blockInterval shouldBe 30.seconds
    s.nodeSettings.headerChainDiff shouldBe 400
    s.chainSettings.powScheme.getClass shouldBe classOf[AutolykosPowScheme]
    s.chainSettings.powScheme.powHeightDivisor shouldBe 4
    s.chainSettings.powScheme.k shouldBe 32
    s.chainSettings.powScheme.n shouldBe 26
    s.chainSettings.yolo.enabled shouldBe true
    s.chainSettings.yolo.softForkVotingStartHeight shouldBe 0
    s.chainSettings.dataInputsUniquenessHeight shouldBe 0
    s.chainSettings.storageRentDistinctOutputsActivationHeight shouldBe 0
    s.launchParameters shouldBe YoloLaunchParameters
    // genesis header has the launch block version (4), not Header.InitialVersion: Autolykos v2 from block 1
    s.launchParameters.blockVersion shouldBe Header.Interpreter60Version
    s.chainSettings.genesisBlockVersion(s.launchParameters.blockVersion) shouldBe Header.Interpreter60Version
    s.launchParameters.storageFeeFactor shouldBe 300000
    s.launchParameters.maxBlockCost shouldBe 1500000
    // EIP-37 epoch 64, and the plain difficulty epoch set to match; genesis difficulty placeholder at the floor
    // (never "01")
    s.chainSettings.eip37EpochLength shouldBe Some(64)
    s.chainSettings.epochLength shouldBe 64
    s.chainSettings.initialDifficultyHex shouldBe "02"
    s.chainSettings.initialDifficulty shouldBe YoloSettings.MinDifficulty

    // values kept from the in-tree testnet.conf
    s.chainSettings.addressPrefix shouldBe 16
    s.chainSettings.protocolVersion shouldBe 4
    // the voting epoch is a separate setting and stays at 128
    s.chainSettings.voting.votingLength shouldBe 128
    s.chainSettings.voting.softForkEpochs shouldBe 32
    s.chainSettings.voting.activationEpochs shouldBe 32
    s.chainSettings.voting.version2ActivationHeight shouldBe Int.MaxValue
    s.chainSettings.monetary.minerRewardDelay shouldBe 720
    s.chainSettings.reemission.checkReemissionRules shouldBe false
  }

  property("yolo-testnet.conf read with --testnet gives the YOLO values") {
    val s = ErgoSettingsReader.readUnchecked(Args(Some(yoloTestnetPath), Some(NetworkType.TestNet)))
    checkYoloTestnet(s)
    s.scorexSettings.network.allowLocal shouldBe false
  }

  property("yolo-testnet.conf read without --testnet gives the same values (it includes testnet.conf)") {
    val s = ErgoSettingsReader.readUnchecked(Args(Some(yoloTestnetPath), None))
    checkYoloTestnet(s)
    s.scorexSettings.network.allowLocal shouldBe false
  }

  property("yolo-devnet.conf is yolo-testnet.conf with local peers allowed") {
    // the full, checked read: the devnet conf passes every start-up check
    val s = ErgoSettingsReader.read(Args(Some(yoloDevnetPath), Some(NetworkType.TestNet)))
    checkYoloTestnet(s)
    s.scorexSettings.network.allowLocal shouldBe true
  }

  property("the upstream test settings have the switch off and divisor 1") {
    settings.chainSettings.yolo.enabled shouldBe false
    settings.chainSettings.powScheme shouldBe a[DefaultFakePowScheme]
    settings.chainSettings.powScheme.powHeightDivisor shouldBe 1
    // rule 308 as upstream: from block version 5 only
    settings.chainSettings.storageRentAttestationRequired(Header.Interpreter60Version) shouldBe false
    settings.chainSettings.storageRentAttestationRequired(Header.StorageRentAttestationVersion) shouldBe true
    // genesis header version as upstream: 1, whatever the launch parameters say
    settings.chainSettings.genesisBlockVersion(Header.Interpreter60Version) shouldBe Header.InitialVersion
  }

  property("start-up check: the shipped YOLO confs and the upstream test settings pass the chain checks") {
    ErgoSettingsReader.yoloSettingsError(ErgoSettingsReader.readUnchecked(Args(Some(yoloTestnetPath), Some(NetworkType.TestNet)))) shouldBe None
    ErgoSettingsReader.yoloSettingsError(ErgoSettingsReader.readUnchecked(Args(Some(yoloTestnetPath), None))) shouldBe None
    ErgoSettingsReader.yoloSettingsError(ErgoSettingsReader.read(Args(Some(yoloDevnetPath), Some(NetworkType.TestNet)))) shouldBe None
    ErgoSettingsReader.yoloSettingsError(settings) shouldBe None
  }

  property("start-up check refuses the YOLO switch on a network type other than testnet") {
    val yolo = ErgoSettingsReader.readUnchecked(Args(Some(yoloTestnetPath), Some(NetworkType.TestNet)))
    Seq(NetworkType.MainNet, NetworkType.DevNet, NetworkType.DevNet60, NetworkType.Tests).foreach { t =>
      val err = ErgoSettingsReader.yoloSettingsError(yolo.copy(networkType = t))
      err shouldBe defined
      err.get should include("networkType")
      err.get should include(t.verboseName)
    }
  }

  property("start-up check refuses the YOLO switch with the Ergo mainnet address prefix") {
    val yolo = ErgoSettingsReader.readUnchecked(Args(Some(yoloTestnetPath), Some(NetworkType.TestNet)))
    val mainnetPrefix = yolo.copy(chainSettings = yolo.chainSettings.copy(addressPrefix = 0))
    mainnetPrefix.chainSettings.isMainnet shouldBe true
    val err = ErgoSettingsReader.yoloSettingsError(mainnetPrefix)
    err shouldBe defined
    err.get should include("addressPrefix")
  }

  property("start-up check refuses a YOLO switch and PoW divisor that disagree") {
    val yolo = ErgoSettingsReader.readUnchecked(Args(Some(yoloTestnetPath), Some(NetworkType.TestNet)))

    // switch on, divisor 1 (a YOLO conf missing powHeightDivisor = 4)
    val noDivisor = yolo.copy(chainSettings = yolo.chainSettings.copy(powScheme = new AutolykosPowScheme(32, 26, 1)))
    val err1 = ErgoSettingsReader.yoloSettingsError(noDivisor)
    err1 shouldBe defined
    err1.get should include("powHeightDivisor = 1")

    // switch off, divisor 4 (e.g. `enabled` misspelt)
    val noSwitch = yolo.copy(chainSettings = yolo.chainSettings.copy(yolo = YoloSettings.disabled))
    val err2 = ErgoSettingsReader.yoloSettingsError(noSwitch)
    err2 shouldBe defined
    err2.get should include("powHeightDivisor = 4")

    // upstream settings with divisor 4 and the switch off
    ErgoSettingsReader.yoloSettingsError(
      settings.copy(chainSettings = settings.chainSettings.copy(powScheme = new DefaultFakePowScheme(32, 26, 4)))) shouldBe defined
  }

  property("start-up check refuses YOLO with the EIP-37 epoch length unset or other than 64") {
    val yolo = ErgoSettingsReader.readUnchecked(Args(Some(yoloTestnetPath), Some(NetworkType.TestNet)))
    YoloSettings.Eip37EpochLength shouldBe 64
    Seq(None, Some(0), Some(1), Some(-64), Some(2), Some(63), Some(65), Some(1024), Some(Int.MaxValue)).foreach { l =>
      val err = ErgoSettingsReader.yoloSettingsError(yolo.copy(chainSettings = yolo.chainSettings.copy(eip37EpochLength = l)))
      withClue(s"eip37EpochLength = $l: ") {
        err shouldBe defined
        err.get should include("eip37EpochLength")
      }
    }
    ErgoSettingsReader.yoloSettingsError(yolo.copy(chainSettings = yolo.chainSettings.copy(eip37EpochLength = Some(64)))) shouldBe None
  }

  property("start-up check refuses a YOLO EIP-37 epoch length of 32 or 128, which would retarget at other heights") {
    val yolo = ErgoSettingsReader.readUnchecked(Args(Some(yoloTestnetPath), Some(NetworkType.TestNet)))
    Seq(32, 128).foreach { l =>
      val err = ErgoSettingsReader.yoloSettingsError(yolo.copy(chainSettings = yolo.chainSettings.copy(eip37EpochLength = Some(l))))
      withClue(s"eip37EpochLength = $l: ") {
        err shouldBe defined
        err.get should include(s"eip37EpochLength = $l")
        err.get should include("epoch length 64")
      }
    }
  }

  property("start-up check refuses YOLO with useLastEpochs below 2, with a message and no division by zero") {
    val yolo = ErgoSettingsReader.readUnchecked(Args(Some(yoloTestnetPath), Some(NetworkType.TestNet)))
    yolo.chainSettings.useLastEpochs shouldBe 8
    Seq(1, 0, -1, Int.MinValue).foreach { n =>
      val err = ErgoSettingsReader.yoloSettingsError(yolo.copy(chainSettings = yolo.chainSettings.copy(useLastEpochs = n)))
      withClue(s"useLastEpochs = $n: ") {
        err shouldBe defined
        err.get should include(s"useLastEpochs = $n")
      }
    }
    ErgoSettingsReader.yoloSettingsError(yolo.copy(chainSettings = yolo.chainSettings.copy(useLastEpochs = 2))) shouldBe None
  }

  property("start-up check refuses a YOLO initial difficulty below the floor of 2, e.g. testnet.conf's 01") {
    val yolo = ErgoSettingsReader.readUnchecked(Args(Some(yoloTestnetPath), Some(NetworkType.TestNet)))
    def withDiff(hex: String): ErgoSettings = yolo.copy(chainSettings = yolo.chainSettings.copy(initialDifficultyHex = hex))

    // "80" and "ff" parse as negative numbers (two's complement)
    Seq("01", "00", "0001", "80", "ff", "ff02").foreach { hex =>
      val err = ErgoSettingsReader.yoloSettingsError(withDiff(hex))
      withClue(s"initialDifficultyHex = $hex: ") {
        err shouldBe defined
        err.get should include("initialDifficultyHex")
        err.get should include("floor")
      }
    }
    // at or above the floor, exactly representable in nBits: accepted (011765000000 is the Ergo mainnet value)
    Seq("02", "0002", "03", "7f", "0080", "0100", "011765000000").foreach { hex =>
      withClue(s"initialDifficultyHex = $hex: ") {
        ErgoSettingsReader.yoloSettingsError(withDiff(hex)) shouldBe None
      }
    }
    // with the switch off the upstream values are not checked: the upstream test settings use "01"
    settings.chainSettings.initialDifficultyHex shouldBe "01"
    ErgoSettingsReader.yoloSettingsError(settings) shouldBe None
    ErgoSettingsReader.yoloSettingsError(
      withDiff("01").copy(chainSettings = withDiff("01").chainSettings.copy(yolo = YoloSettings.disabled,
        powScheme = new AutolykosPowScheme(32, 26, 1)))) shouldBe None
  }

  property("start-up check refuses a YOLO initial difficulty that nBits cannot represent exactly") {
    val yolo = ErgoSettingsReader.readUnchecked(Args(Some(yoloTestnetPath), Some(NetworkType.TestNet)))
    def withDiff(hex: String): ErgoSettings = yolo.copy(chainSettings = yolo.chainSettings.copy(initialDifficultyHex = hex))
    // 4 significant bytes: nBits keeps 3, so the genesis header could never carry this difficulty
    Seq("01234567", "011765000001", "7fffffff").foreach { hex =>
      val err = ErgoSettingsReader.yoloSettingsError(withDiff(hex))
      withClue(s"initialDifficultyHex = $hex: ") {
        err shouldBe defined
        err.get should include("representable")
      }
    }
    ErgoSettingsReader.yoloSettingsError(withDiff("01234500")) shouldBe None
  }

  property("unknown keys in the yolo block are refused, e.g. the `enable` typo") {
    def check(s: String): Option[String] = ErgoSettingsReader.unknownYoloKeysError(ConfigFactory.parseString(s))

    val typo = check("ergo.chain.yolo { enable = true }")
    typo shouldBe defined
    typo.get should include("ergo.chain.yolo.enable")

    check("ergo.chain { }") shouldBe None
    check("ergo.chain.yolo { }") shouldBe None
  }

  property("the shipped yolo blocks (yolo-testnet.conf, yolo-devnet.conf, main application.conf) have no unknown keys") {
    ErgoSettingsReader.unknownYoloKeysError(ConfigFactory.parseFile(new File(yoloTestnetPath))) shouldBe None
    ErgoSettingsReader.unknownYoloKeysError(ConfigFactory.parseFile(new File(yoloDevnetPath))) shouldBe None

    // the main application.conf is shadowed on the test classpath by the test one, so find it explicitly
    val appConfs = getClass.getClassLoader.getResources("application.conf").asScala.toSeq
    val mainAppConf = appConfs.find(u => !u.toString.contains("test-classes"))
      .getOrElse(fail(s"main application.conf not found among $appConfs"))
    val mainCfg = ConfigFactory.parseURL(mainAppConf)
    mainCfg.hasPath("ergo.chain.yolo.enabled") shouldBe true
    ErgoSettingsReader.unknownYoloKeysError(mainCfg) shouldBe None
  }

  property("launch parameters: upstream selection with the switch off, YOLO parameters with it on") {
    def withType(t: NetworkType): ErgoSettings = settings.copy(networkType = t)

    withType(NetworkType.MainNet).launchParameters shouldBe MainnetLaunchParameters
    withType(NetworkType.TestNet).launchParameters shouldBe TestnetLaunchParameters
    withType(NetworkType.DevNet).launchParameters shouldBe DevnetLaunchParameters
    withType(NetworkType.DevNet60).launchParameters shouldBe Devnet60LaunchParameters
    withType(NetworkType.Tests).launchParameters shouldBe MainnetLaunchParameters

    val yoloChain = settings.chainSettings.copy(yolo = YoloSettings(enabled = true))
    Seq(NetworkType.MainNet, NetworkType.TestNet, NetworkType.DevNet, NetworkType.DevNet60, NetworkType.Tests)
      .foreach { t =>
        withType(t).copy(chainSettings = yoloChain).launchParameters shouldBe YoloLaunchParameters
      }
  }

}
