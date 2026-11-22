package org.ergoplatform.settings

import com.typesafe.config.ConfigFactory
import org.ergoplatform.mining.{AutolykosPowScheme, CandidateGenerator, DefaultFakePowScheme}
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
    s.nodeSettings.rejectStorageRentTxs shouldBe true
    // miner self-collection of storage rent, which needs the extra index
    s.nodeSettings.storageRentCollection shouldBe true
    s.nodeSettings.extraIndex shouldBe true
    s.nodeSettings.isFullBlocksPruned shouldBe false
    s.chainSettings.reemission.activationHeight shouldBe Int.MaxValue
    s.chainSettings.powScheme.getClass shouldBe classOf[AutolykosPowScheme]
    s.chainSettings.powScheme.powHeightDivisor shouldBe 4
    s.chainSettings.powScheme.k shouldBe 32
    s.chainSettings.powScheme.n shouldBe 26
    s.chainSettings.yolo.enabled shouldBe true
    s.chainSettings.yolo.softForkVotingStartHeight shouldBe 0
    s.chainSettings.dataInputsUniquenessHeight shouldBe 0
    s.chainSettings.storageRentDistinctOutputsActivationHeight shouldBe 0
    // rule 308 (bsStorageRentAttestation) from genesis, whatever the block version
    s.chainSettings.storageRentAttestationRequired(Header.InitialVersion) shouldBe true
    s.chainSettings.storageRentAttestationRequired(Header.Interpreter60Version) shouldBe true
    s.launchParameters shouldBe YoloLaunchParameters
    // genesis header has the launch block version (4), not Header.InitialVersion: Autolykos v2 from block 1
    s.launchParameters.blockVersion shouldBe Header.Interpreter60Version
    s.chainSettings.genesisBlockVersion(s.launchParameters.blockVersion) shouldBe Header.Interpreter60Version
    s.launchParameters.storageFeeFactor shouldBe 300000
    // block cost limit 1,500,000 at genesis; mempool relay limit 1,000,000 (testnet.conf: 800,000), within the room
    // a full block leaves for one pool transaction, so no start-up warning
    s.launchParameters.maxBlockCost shouldBe 1500000
    s.nodeSettings.maxTransactionCost shouldBe 1000000
    CandidateGenerator.yoloRelayLimitWarning(s) shouldBe None
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

  // yolo-testnet.conf ships no genesis inputs: the emission block is unset and the digest is testnet.conf's
  private def checkYoloTestnetGenesisUnset(s: ErgoSettings): Unit = {
    s.chainSettings.yolo.emission shouldBe YoloEmissionSettings()
    s.chainSettings.genesisStateDigestHex shouldBe "cb63aa99a3060f341781d8662b58bf18b9ad258db4fe88d09f8f71cb668cad4502"
  }

  // read without the start-up checks: a node started on yolo-testnet.conf alone refuses (genesis inputs unset),
  // and a refusal exits the JVM
  property("yolo-testnet.conf read with --testnet gives the YOLO values") {
    val s = ErgoSettingsReader.readUnchecked(Args(Some(yoloTestnetPath), Some(NetworkType.TestNet)))
    checkYoloTestnet(s)
    checkYoloTestnetGenesisUnset(s)
    s.scorexSettings.network.allowLocal shouldBe false
  }

  property("yolo-testnet.conf read without --testnet gives the same values (it includes testnet.conf)") {
    val s = ErgoSettingsReader.readUnchecked(Args(Some(yoloTestnetPath), None))
    checkYoloTestnet(s)
    checkYoloTestnetGenesisUnset(s)
    s.scorexSettings.network.allowLocal shouldBe false
  }

  property("yolo-devnet.conf is yolo-testnet.conf with local peers allowed and the devnet genesis inputs") {
    // the full, checked read: the devnet conf passes every start-up check, the YOLO launch check included
    val s = ErgoSettingsReader.read(Args(Some(yoloDevnetPath), Some(NetworkType.TestNet)))
    checkYoloTestnet(s)
    s.scorexSettings.network.allowLocal shouldBe true
    val e = s.chainSettings.yolo.emission
    e.emissionNftId shouldBe "44bc63258d371f08ba0ac4b87eb356cf8e84dbfa31afef89a5b4a953cabdad2a"
    e.treasuryNftId shouldBe "26adf62f0d439cd5cccaad2343e2768085d7def9a0230a9251ce8477581f9148"
    e.lpNftId shouldBe "d6f3e22aca5641481ec05e2f134f451bfe11e489820eb149181e59eae7ce6967"
    e.genesisValue shouldBe 177291250000000000L
    e.treasuryFundValue shouldBe 100000000000L
    e.lpFundValue shouldBe 100000000000L
    e.lpWhitelist shouldBe Seq.empty
    // the devnet fund trees hold the public test signer keys, which only the devnet allows
    e.allowTestSignerKeys shouldBe true
    Seq(e.emissionTreeHex, e.treasuryAccumulationTreeHex, e.lpAccumulationTreeHex, e.treasuryFundTreeHex,
      e.lpFundTreeHex).foreach(_ should not be empty)
    s.chainSettings.genesisStateDigestHex should not be
      "cb63aa99a3060f341781d8662b58bf18b9ad258db4fe88d09f8f71cb668cad4502"
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

  property("start-up check: yolo-devnet.conf and the upstream test settings pass the YOLO launch check; " +
    "yolo-testnet.conf is refused and the message names every missing input") {
    ErgoSettingsReader.yoloGenesisError(ErgoSettingsReader.read(Args(Some(yoloDevnetPath), Some(NetworkType.TestNet)))) shouldBe None
    ErgoSettingsReader.yoloGenesisError(settings) shouldBe None

    Seq(Some(NetworkType.TestNet), None).foreach { t =>
      val err = ErgoSettingsReader.yoloGenesisError(ErgoSettingsReader.readUnchecked(Args(Some(yoloTestnetPath), t)))
      err shouldBe defined
      err.get should include("ergo.chain.yolo.emission")
      Seq("emissionTreeHex", "treasuryAccumulationTreeHex", "lpAccumulationTreeHex", "treasuryFundTreeHex",
        "lpFundTreeHex", "emissionNftId", "treasuryNftId", "lpNftId").foreach { field =>
        err.get should include(s"$field: unset")
      }
    }
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

  property("start-up check refuses YOLO with EIP-27 re-emission rules checked (checkReemissionRules = true)") {
    // with the rules checked and activationHeight = 1, the honest height-1 YOLO coinbase is rejected (it pays no
    // re-emission tokens), so the switch on requires checkReemissionRules = false
    val yolo = ErgoSettingsReader.readUnchecked(Args(Some(yoloTestnetPath), Some(NetworkType.TestNet)))
    yolo.chainSettings.reemission.checkReemissionRules shouldBe false
    def withCheck(check: Boolean, height: Int): ErgoSettings = yolo.copy(chainSettings = yolo.chainSettings.copy(
      reemission = yolo.chainSettings.reemission.copy(checkReemissionRules = check, activationHeight = height)))
    Seq(Int.MaxValue, 1).foreach { h =>
      val err = ErgoSettingsReader.yoloSettingsError(withCheck(check = true, h))
      withClue(s"activationHeight = $h: ") {
        err shouldBe defined
        err.get should include("ergo.chain.reemission.checkReemissionRules = true")
      }
    }
    ErgoSettingsReader.yoloSettingsError(withCheck(check = false, Int.MaxValue)) shouldBe None
    // switch off: upstream, not checked here
    ErgoSettingsReader.yoloSettingsError(settings.copy(chainSettings = settings.chainSettings.copy(
      reemission = settings.chainSettings.reemission.copy(checkReemissionRules = true)))) shouldBe None
  }

  property("start-up check refuses YOLO with an EIP-27 activation height other than Int.MaxValue") {
    val yolo = ErgoSettingsReader.readUnchecked(Args(Some(yoloTestnetPath), Some(NetworkType.TestNet)))
    yolo.chainSettings.reemission.activationHeight shouldBe Int.MaxValue
    Seq(1, 0, 777217, 100000001, Int.MaxValue - 1).foreach { h =>
      val err = ErgoSettingsReader.yoloSettingsError(yolo.copy(chainSettings = yolo.chainSettings.copy(
        reemission = yolo.chainSettings.reemission.copy(activationHeight = h))))
      withClue(s"activationHeight = $h: ") {
        err shouldBe defined
        err.get should include(s"ergo.chain.reemission.activationHeight = $h")
      }
    }
    // switch off: upstream values (the upstream test settings activate EIP-27 at a finite height)
    settings.chainSettings.reemission.activationHeight should not be Int.MaxValue
    ErgoSettingsReader.yoloSettingsError(settings) shouldBe None
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

    val emissionTypo = check("ergo.chain.yolo { enabled = true, emission { genesisValu = 1 } }")
    emissionTypo shouldBe defined
    emissionTypo.get should include("ergo.chain.yolo.emission.genesisValu")

    // the old key names treasuryTreeHex and lpTreeHex are refused, so an old config cannot silently drop its
    // accumulation trees
    val renamed = check("ergo.chain.yolo { emission { treasuryTreeHex = \"\", lpTreeHex = \"\" } }")
    renamed shouldBe defined
    renamed.get should include("ergo.chain.yolo.emission.treasuryTreeHex")
    renamed.get should include("ergo.chain.yolo.emission.lpTreeHex")

    // whitelist entries: unknown keys in an entry are refused too
    val entryTypo = check("ergo.chain.yolo { emission { lpWhitelist = [ { hash = \"00\", heigth = 0 } ] } }")
    entryTypo shouldBe defined
    entryTypo.get should include("ergo.chain.yolo.emission.lpWhitelist.0.heigth")
    val secondEntry = check(
      "ergo.chain.yolo { emission { lpWhitelist = [ { hash = \"00\", height = 0 }, { hash = \"00\", h = 0 } ] } }")
    secondEntry shouldBe defined
    secondEntry.get should include("ergo.chain.yolo.emission.lpWhitelist.1.h")

    check("ergo.chain { }") shouldBe None
    check("ergo.chain.yolo { }") shouldBe None
    check(
      """ergo.chain.yolo {
        |  enabled = true
        |  softForkVotingStartHeight = 0
        |  emission {
        |    emissionTreeHex = "", treasuryAccumulationTreeHex = "", lpAccumulationTreeHex = ""
        |    treasuryFundTreeHex = "", lpFundTreeHex = ""
        |    emissionNftId = "", treasuryNftId = "", lpNftId = ""
        |    genesisValue = 0, treasuryFundValue = 0, lpFundValue = 0
        |    lpWhitelist = [ { hash = "00", height = 0 } ]
        |    allowTestSignerKeys = false
        |  }
        |}""".stripMargin) shouldBe None
  }

  property("start-up check: the public test signer keys are refused unless allowTestSignerKeys = true, " +
    "which only yolo-devnet.conf sets") {
    val devnet = ErgoSettingsReader.read(Args(Some(yoloDevnetPath), Some(NetworkType.TestNet)))
    devnet.chainSettings.yolo.emission.allowTestSignerKeys shouldBe true
    ErgoSettingsReader.yoloGenesisError(devnet) shouldBe None

    // the same genesis inputs with the setting off, as on any network other than the local devnet
    val off = devnet.copy(chainSettings = devnet.chainSettings.copy(yolo = devnet.chainSettings.yolo.copy(
      emission = devnet.chainSettings.yolo.emission.copy(allowTestSignerKeys = false))))
    val err = ErgoSettingsReader.yoloGenesisError(off)
    err shouldBe defined
    err.get should include("treasuryFundTreeHex holds a public test signer key")
    err.get should include("lpFundTreeHex holds a public test signer key")
    err.get should include("ergo.chain.yolo.emission.allowTestSignerKeys = true")

    // off by default: yolo-testnet.conf leaves it unset, and the main application.conf says false
    ErgoSettingsReader.readUnchecked(Args(Some(yoloTestnetPath), Some(NetworkType.TestNet)))
      .chainSettings.yolo.emission.allowTestSignerKeys shouldBe false
    YoloEmissionSettings().allowTestSignerKeys shouldBe false
    val mainAppConf = getClass.getClassLoader.getResources("application.conf").asScala.toSeq
      .find(u => !u.toString.contains("test-classes")).get
    ConfigFactory.parseURL(mainAppConf).getBoolean("ergo.chain.yolo.emission.allowTestSignerKeys") shouldBe false
    ConfigFactory.parseFile(new File(yoloTestnetPath)).hasPath("ergo.chain.yolo.emission.allowTestSignerKeys") shouldBe
      false
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

  property("start-up warning: a relay limit above the room a full block leaves (block cost limit of the launch " +
    "parameters - safe gap - reserve for the mandatory txs) is warned of with the switch on, never with it off") {
    // the assembly safe gap tiers
    CandidateGenerator.safeGap(999999) shouldBe 0
    CandidateGenerator.safeGap(1000000) shouldBe 150000
    CandidateGenerator.safeGap(4999999) shouldBe 150000
    CandidateGenerator.safeGap(5000000) shouldBe 500000
    CandidateGenerator.YoloMandatoryTxsCostReserve shouldBe 300000
    CandidateGenerator.YoloRentClaimCostBudget shouldBe 270000

    val s = ErgoSettingsReader.readUnchecked(Args(Some(yoloTestnetPath), Some(NetworkType.TestNet)))
    s.chainSettings.yolo.enabled shouldBe true
    def withLimit(x: Int, on: Boolean = true): ErgoSettings = s.copy(
      nodeSettings = s.nodeSettings.copy(maxTransactionCost = x),
      chainSettings = s.chainSettings.copy(yolo = s.chainSettings.yolo.copy(enabled = on)))
    // 1,500,000 - 150,000 - 300,000 - 2
    val room = 1049998

    // the shipped value, and up to the room: no warning
    CandidateGenerator.yoloRelayLimitWarning(s) shouldBe None
    CandidateGenerator.yoloRelayLimitWarning(withLimit(room)) shouldBe None
    // one above the room, and 1,200,000: warned of, with the numbers
    Seq(room + 1, 1200000).foreach { x =>
      val w = CandidateGenerator.yoloRelayLimitWarning(withLimit(x))
      w shouldBe defined
      w.get should include(s"ergo.node.maxTransactionCost = $x is above the $room a full block leaves")
      w.get should include("block cost limit 1500000 of the launch parameters")
      w.get should include("safe gap 150000")
      w.get should include("minus 300000 kept for the emission transaction, a storage rent claim within its cost " +
        "budget of 270000 and the fee transaction, minus 2")
      w.get should include(s"Set maxTransactionCost to $room or less")
    }
    // switch off: never, whatever the value (upstream has no such warning)
    CandidateGenerator.yoloRelayLimitWarning(withLimit(1200000, on = false)) shouldBe None
    CandidateGenerator.yoloRelayLimitWarning(settings) shouldBe None
  }

  property("relay limit check against the block cost limit in use: warned of once votes move the limit below " +
    "what the relay limit needs, never at the launch value, never with the switch off") {
    val s = ErgoSettingsReader.readUnchecked(Args(Some(yoloTestnetPath), Some(NetworkType.TestNet)))
    s.nodeSettings.maxTransactionCost shouldBe 1000000
    s.launchParameters.maxBlockCost shouldBe 1500000
    def off: ErgoSettings = s.copy(chainSettings = s.chainSettings.copy(yolo = s.chainSettings.yolo.copy(enabled = false)))

    // the launch value is the start-up warning's business
    CandidateGenerator.yoloVotedRelayLimitWarning(s, 1500000) shouldBe None
    // voted up, or down while the relay limit still fits (room = limit - 150,000 - 300,000 - 2 >= 1,000,000)
    CandidateGenerator.yoloVotedRelayLimitWarning(s, 1515000) shouldBe None
    CandidateGenerator.yoloVotedRelayLimitWarning(s, 1450002) shouldBe None
    // voted to 1,450,001 or below: the room drops under the relay limit
    val w = CandidateGenerator.yoloVotedRelayLimitWarning(s, 1450001)
    w shouldBe defined
    w.get should include("ergo.node.maxTransactionCost = 1000000 is above the 999999 a full block leaves")
    w.get should include("block cost limit 1450001 in use (voted; launch value 1500000)")
    w.get should include("Set maxTransactionCost to 999999 or less")
    // the same pure message the start-up warning uses
    w shouldBe CandidateGenerator.yoloRelayLimitMessage(1000000, 1450001, "in use (voted; launch value 1500000)")
    // switch off: never
    CandidateGenerator.yoloVotedRelayLimitWarning(off, 1450001) shouldBe None
    CandidateGenerator.yoloVotedRelayLimitWarning(off, 1000000) shouldBe None
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
