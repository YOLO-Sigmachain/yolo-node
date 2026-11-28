package org.ergoplatform.nodeView.state

import org.ergoplatform.ErgoBox
import org.ergoplatform.ErgoBox.{R4, R5, R6, R7, R8, R9}
import org.ergoplatform.mining.emission.YoloEmissionRules
import com.typesafe.config.ConfigFactory
import org.ergoplatform.settings.{Algos, Args, ChainSettingsReader, ErgoSettings, ErgoSettingsReader, NetworkType, YoloEmissionSettings,
  YoloLaunchParameters, YoloSettings, YoloWhitelistEntry}
import org.ergoplatform.settings.Algos.HF
import org.ergoplatform.settings.Constants.TrueTree
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.wallet.boxes.ErgoBoxSerializer
import org.ergoplatform.wallet.utils.TestFileUtils
import scorex.crypto.authds.ADValue
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, Insert}
import scorex.crypto.hash.Digest32
import scorex.util.encode.Base16
import sigma.{Coll, Colls}
import sigma.ast.{ByteArrayConstant, CollectionConstant, IntConstant, LongConstant, SByte, SCollection, SLong, STuple}
import sigma.data.{CBox, Digest32Coll}
import sigma.exceptions.InvalidType

import java.io.File
import scala.collection.JavaConverters._

/**
  * YOLO genesis state, launch check (one passing config and refusing mutations for each condition, 1 to 8
  * with 4b the contract identity pins and 4c the public test signer keys), and the devnet genesis state digest.
  */
class YoloGenesisSpec extends ErgoCorePropertyTest with TestFileUtils {
  import org.ergoplatform.utils.ErgoNodeTestConstants._

  private lazy val devnetPath: String =
    new File(getClass.getClassLoader.getResource("yolo-devnet.conf").toURI).getAbsolutePath

  // the full, checked read: exits the JVM if the devnet conf fails a start-up check
  private lazy val devnet: ErgoSettings = ErgoSettingsReader
    .read(Args(Some(devnetPath), Some(NetworkType.TestNet)))
    .copy(directory = createTempDir.getAbsolutePath) // states open a snapshots db under it
  private lazy val e: YoloEmissionSettings = devnet.chainSettings.yolo.emission

  private def withEmission(f: YoloEmissionSettings => YoloEmissionSettings): ErgoSettings =
    devnet.copy(chainSettings = devnet.chainSettings.copy(
      yolo = devnet.chainSettings.yolo.copy(emission = f(devnet.chainSettings.yolo.emission))))

  private def launchError(f: YoloEmissionSettings => YoloEmissionSettings): Option[String] =
    YoloGenesis.launchError(withEmission(f).chainSettings)

  // the chain settings with genesisStateDigestHex set to the digest of their genesis boxes, for a mutation that
  // changes a genesis box and is otherwise accepted (condition 8 refuses a stale digest)
  private def withOwnDigest(s: ErgoSettings): ErgoSettings = s.copy(chainSettings = s.chainSettings.copy(
    genesisStateDigestHex = Base16.encode(YoloGenesis.stateDigest(ErgoState.genesisBoxes(s.chainSettings)))))

  private def refused(f: YoloEmissionSettings => YoloEmissionSettings, expected: String*): Unit = {
    val err = launchError(f)
    err shouldBe defined
    expected.foreach(x => err.get should include(x))
  }

  // AVL+ digest of a genesis box set, as UtxoState.fromBoxHolder computes it
  private def digestOf(boxes: Seq[ErgoBox]): String = {
    val p = new BatchAVLProver[Digest32, HF](keyLength = 32, valueLengthOpt = None)
    BoxHolder(boxes).sortedBoxes.foreach(b => p.performOneOperation(Insert(b.id, ADValue @@ b.bytes)).get)
    Base16.encode(p.digest)
  }

  private def roundTrip(b: ErgoBox): ErgoBox = {
    val parsed = ErgoBoxSerializer.parseBytes(ErgoBoxSerializer.toBytes(b))
    parsed.bytes.toSeq shouldBe b.bytes.toSeq
    parsed
  }

  private val Zero32: Array[Byte] = Array.fill(32)(0: Byte)
  private val WhitelistType = SCollection(STuple(SCollection(SByte), SLong))

  private def token(id: Array[Byte], amount: Long = 1L): (ErgoBox.TokenId, Long) =
    (Digest32Coll @@ Colls.fromArray(id)) -> amount

  // blake2b256 of the devnet accumulation trees, from the tree compiler output
  private val TreasuryAccumulationHash = "65f06d8d0986c6655ba90e137d597bedf7cdbaf31e5d9020c140dcdab0161763"
  private val LpAccumulationHash = "90482c952d0eb7c083da506892c5921429618c64f41b31a3e0c83dddfa655582"

  property("the devnet config passes the launch check") {
    YoloGenesis.launchError(devnet.chainSettings) shouldBe None
    devnet.chainSettings.yolo.enabled shouldBe true
  }

  property("genesis order: emission, treasury fund, LP fund, no-premine; no founders box") {
    val boxes = ErgoState.genesisBoxes(devnet.chainSettings)
    boxes.size shouldBe 4
    boxes.head shouldBe YoloGenesis.emissionBox(e)
    boxes(1) shouldBe YoloGenesis.treasuryFundBox(e)
    boxes(2) shouldBe YoloGenesis.lpFundBox(e)
    boxes(3) shouldBe ErgoState.noPremineBox(devnet.chainSettings)
    boxes(3).ergoTree shouldBe org.ergoplatform.settings.Constants.FalseTree
    boxes.foreach(_.creationHeight shouldBe 0)
    // nothing at the Ergo emission or founders scripts
    boxes.foreach { b =>
      b.ergoTree should not be devnet.chainSettings.monetary.emissionBoxProposition
    }
    // 177,291,250 YOLO emission box and two 100 YOLO fund boxes: the 177,291,450 YOLO schedule supply
    boxes.map(_.value).sum shouldBe
      (177291250000000000L + 2 * 100000000000L + org.ergoplatform.mining.emission.EmissionRules.CoinsInOneErgo)
    boxes.take(3).map(_.value).sum shouldBe 177291450000000000L
  }

  property("emission box layout, decoded back from its bytes: value, tree, tokens(0), R4 and R5 Coll[Byte] only") {
    val b = roundTrip(ErgoState.genesisBoxes(devnet.chainSettings).head)
    b.value shouldBe 177291250000000000L
    Base16.encode(b.propositionBytes) shouldBe e.emissionTreeHex
    b.creationHeight shouldBe 0
    b.additionalTokens.length shouldBe 1
    Base16.encode(b.additionalTokens(0)._1.toArray) shouldBe e.emissionNftId
    b.additionalTokens(0)._2 shouldBe 1L
    b.additionalRegisters.keySet shouldBe Set(R4, R5)
    b.additionalRegisters(R4).tpe shouldBe SCollection(SByte)
    b.additionalRegisters(R5).tpe shouldBe SCollection(SByte)
    b.additionalRegisters(R4) shouldBe ByteArrayConstant(Base16.decode(TreasuryAccumulationHash).get)
    b.additionalRegisters(R5) shouldBe ByteArrayConstant(Base16.decode(LpAccumulationHash).get)
    TreasuryAccumulationHash shouldBe Base16.encode(Algos.hash(Base16.decode(e.treasuryAccumulationTreeHex).get))
    LpAccumulationHash shouldBe Base16.encode(Algos.hash(Base16.decode(e.lpAccumulationTreeHex).get))
    // the interpreter's register read (SELF.R4[Coll[Byte]] in emission.es)
    val c = CBox(b)
    c.getReg[Coll[Byte]](4).map(_.toArray.toSeq) shouldBe Some(Base16.decode(TreasuryAccumulationHash).get.toSeq)
    c.getReg[Coll[Byte]](5).map(_.toArray.toSeq) shouldBe Some(Base16.decode(LpAccumulationHash).get.toSeq)
    c.getReg[Coll[Byte]](6) shouldBe None
    an[InvalidType] should be thrownBy c.getReg[Long](4)
    // tracked as the emission box
    e.isEmissionBox(b) shouldBe true
  }

  property("treasury fund box layout, decoded back: R4 Coll[Byte] 32 zeros, R5 Long 0, R6 Coll[Byte] empty, R7 and R8 Long 0") {
    val b = roundTrip(ErgoState.genesisBoxes(devnet.chainSettings)(1))
    b.value shouldBe 100000000000L
    Base16.encode(b.propositionBytes) shouldBe e.treasuryFundTreeHex
    b.additionalTokens.length shouldBe 1
    Base16.encode(b.additionalTokens(0)._1.toArray) shouldBe e.treasuryNftId
    b.additionalTokens(0)._2 shouldBe 1L
    b.additionalRegisters.keySet shouldBe Set(R4, R5, R6, R7, R8)
    b.additionalRegisters(R4) shouldBe ByteArrayConstant(Zero32)
    b.additionalRegisters(R5) shouldBe LongConstant(0L)
    b.additionalRegisters(R6) shouldBe ByteArrayConstant(Array.emptyByteArray)
    b.additionalRegisters(R7) shouldBe LongConstant(0L)
    b.additionalRegisters(R8) shouldBe LongConstant(0L)
    Seq(R4, R6).foreach(r => b.additionalRegisters(r).tpe shouldBe SCollection(SByte))
    Seq(R5, R7, R8).foreach(r => b.additionalRegisters(r).tpe shouldBe SLong)
    // the interpreter's register reads in treasury_governance.es (SELF.R4[Coll[Byte]] ... SELF.R8[Long])
    val c = CBox(b)
    c.getReg[Coll[Byte]](4).map(_.toArray.toSeq) shouldBe Some(Zero32.toSeq)
    c.getReg[Long](5) shouldBe Some(0L)
    c.getReg[Coll[Byte]](6).map(_.length) shouldBe Some(0)
    c.getReg[Long](7) shouldBe Some(0L)
    c.getReg[Long](8) shouldBe Some(0L)
    an[InvalidType] should be thrownBy c.getReg[Int](5)
  }

  property("LP fund box layout, decoded back: R4 Coll[(Coll[Byte], Long)] (also empty), R5 and R6 Long 0, R7 32 zeros") {
    val entries = Seq(YoloWhitelistEntry("ab" * 32, 0L), YoloWhitelistEntry("cd" * 32, 0L))
    Seq(Seq.empty[YoloWhitelistEntry], entries).foreach { wl =>
      withClue(s"whitelist of ${wl.size}: ") {
        val s = withEmission(_.copy(lpWhitelist = wl))
        YoloGenesis.launchError(withOwnDigest(s).chainSettings) shouldBe None
        val b = roundTrip(ErgoState.genesisBoxes(s.chainSettings)(2))
        b.value shouldBe 100000000000L
        Base16.encode(b.propositionBytes) shouldBe e.lpFundTreeHex
        b.additionalTokens.length shouldBe 1
        Base16.encode(b.additionalTokens(0)._1.toArray) shouldBe e.lpNftId
        b.additionalTokens(0)._2 shouldBe 1L
        b.additionalRegisters.keySet shouldBe Set(R4, R5, R6, R7)
        b.additionalRegisters(R4).tpe shouldBe WhitelistType
        b.additionalRegisters(R5) shouldBe LongConstant(0L)
        b.additionalRegisters(R6) shouldBe LongConstant(0L)
        b.additionalRegisters(R7) shouldBe ByteArrayConstant(Zero32)
        // the interpreter's register reads in lp_fund.es
        val c = CBox(b)
        val read = c.getReg[Coll[(Coll[Byte], Long)]](4).get.toArray.toSeq.map { case (h, height) =>
          (Base16.encode(h.toArray), height)
        }
        read shouldBe wl.map(w => (w.hash, w.height))
        c.getReg[Long](5) shouldBe Some(0L)
        c.getReg[Long](6) shouldBe Some(0L)
        c.getReg[Coll[Byte]](7).map(_.toArray.toSeq) shouldBe Some(Zero32.toSeq)
        an[InvalidType] should be thrownBy c.getReg[Coll[Coll[Byte]]](4)
        an[InvalidType] should be thrownBy c.getReg[Coll[Byte]](4)
      }
    }
  }

  // ------------------------------------------------------------------ launch check, conditions 1 to 8, 4b and 4c

  property("launch check 1: every tree parses and every hex or id field is well formed") {
    YoloGenesis.inputsError(e) shouldBe None
    refused(_.copy(emissionTreeHex = ""), "emissionTreeHex: unset")
    refused(_.copy(lpFundTreeHex = "zz"), "lpFundTreeHex: not an ErgoTree")
    refused(x => x.copy(treasuryAccumulationTreeHex = x.treasuryAccumulationTreeHex.dropRight(2)),
      "treasuryAccumulationTreeHex: not an ErgoTree")
    refused(x => x.copy(lpAccumulationTreeHex = x.lpAccumulationTreeHex + "00"), "lpAccumulationTreeHex: not an ErgoTree")
    refused(x => x.copy(treasuryFundTreeHex = x.treasuryFundTreeHex.drop(2)), "treasuryFundTreeHex: not an ErgoTree")
    refused(_.copy(treasuryNftId = "ab" * 31), "treasuryNftId: not a 32-byte hex id")
    refused(_.copy(lpNftId = "ab" * 33), "lpNftId: not a 32-byte hex id")
    refused(_.copy(emissionNftId = ""), "emissionNftId: unset")
    refused(_.copy(lpWhitelist = Seq(YoloWhitelistEntry("xyz", 0L))), "lpWhitelist(0).hash")
    // every problem is named at once
    refused(_ => YoloEmissionSettings(), "emissionTreeHex: unset", "lpFundTreeHex: unset", "lpNftId: unset")
  }

  property("launch check 2: NFT ids pairwise distinct and not repeated-byte placeholders") {
    YoloGenesis.idsError(e) shouldBe None
    refused(_.copy(emissionNftId = "cc" * 32), "placeholder", "emissionNftId")
    refused(_.copy(treasuryNftId = "aa" * 32), "placeholder", "treasuryNftId")
    refused(_.copy(lpNftId = "bb" * 32), "placeholder", "lpNftId")
    refused(_.copy(lpNftId = "00" * 32), "placeholder")
    refused(x => x.copy(lpNftId = x.treasuryNftId), "distinct", "treasuryNftId = lpNftId")
    refused(x => x.copy(treasuryNftId = x.emissionNftId), "distinct", "emissionNftId = treasuryNftId")
  }

  property("launch check 3: no NFT id equals a genesis box id") {
    val boxes = ErgoState.genesisBoxes(devnet.chainSettings)
    YoloGenesis.boxIdCollisionError(e, boxes) shouldBe None
    // the treasury fund box does not depend on the LP id, so the LP id can be set to its id
    val treasuryBoxId = Base16.encode(boxes(1).id)
    refused(_.copy(lpNftId = treasuryBoxId), "genesis box id", "lpNftId equals the id of genesis box 1")
    val noPremineId = Base16.encode(boxes(3).id)
    refused(_.copy(treasuryNftId = noPremineId), "treasuryNftId equals the id of genesis box 3")
    // direct: an id equal to the emission box id
    YoloGenesis.boxIdCollisionError(e.copy(emissionNftId = Base16.encode(boxes.head.id)), boxes).get should
      include("emissionNftId equals the id of genesis box 0")
  }

  property("launch check 4: each tree embeds its own NFT id and none of the other two") {
    YoloGenesis.treeIdsError(e) shouldBe None
    // the emission tree replaced by the LP fund tree: embeds the LP id, not the emission id
    refused(x => x.copy(emissionTreeHex = x.lpFundTreeHex), "emissionTreeHex embeds the LP NFT id")
    // accumulation trees swapped: the treasury share would be paid to the LP stream and back
    refused(x => x.copy(treasuryAccumulationTreeHex = x.lpAccumulationTreeHex,
      lpAccumulationTreeHex = x.treasuryAccumulationTreeHex),
      "treasuryAccumulationTreeHex embeds the LP NFT id", "lpAccumulationTreeHex embeds the treasury NFT id")
    // fund trees swapped
    refused(x => x.copy(treasuryFundTreeHex = x.lpFundTreeHex, lpFundTreeHex = x.treasuryFundTreeHex),
      "treasuryFundTreeHex embeds the LP NFT id")
    // a tree with no id at all
    val trueTree = Base16.encode(TrueTree.bytes)
    refused(_.copy(emissionTreeHex = trueTree), "emissionTreeHex does not embed the emission NFT id")
    refused(_.copy(lpFundTreeHex = trueTree), "lpFundTreeHex does not embed the LP NFT id")
    // the trees compiled for other ids (here: the configured ids rotated)
    refused(x => x.copy(emissionNftId = x.treasuryNftId, treasuryNftId = x.lpNftId, lpNftId = x.emissionNftId),
      "embeds")
    // the same tree twice (an accumulation tree given as the fund tree passes the id test, but not this one)
    refused(x => x.copy(treasuryFundTreeHex = x.treasuryAccumulationTreeHex),
      "the same tree twice: treasuryAccumulationTreeHex = treasuryFundTreeHex")
  }

  // a tree with constant i replaced (the root, with its placeholders, kept)
  private def withConstant(hex: String, i: Int, c: sigma.ast.Constant[sigma.ast.SType]): String = {
    val t = YoloEmissionSettings.parseTree(hex).get
    sigma.VersionContext.withVersions(YoloEmissionSettings.ScriptVersion, YoloEmissionSettings.ScriptVersion) {
      Base16.encode(new sigma.ast.ErgoTree(t.header, t.constants.updated(i, c), t.root).bytes)
    }
  }

  private def key(secret: Int): sigma.ast.Constant[sigma.ast.SType] =
    sigma.ast.SigmaPropConstant(sigmastate.crypto.DLogProtocol.DLogProverInput(BigInt(secret).bigInteger).publicImage)
      .asInstanceOf[sigma.ast.Constant[sigma.ast.SType]]

  property("launch check 4b: each tree is its role's pinned contract; only its NFT id and fund signer keys are free") {
    YoloGenesis.treePinsError(e) shouldBe None
    YoloTreePins.TreasuryFund.keyIndices shouldBe Seq(11, 12, 13)
    YoloTreePins.LpFund.keyIndices shouldBe Seq(12, 13, 14)

    // the accumulation and fund trees of one fund swapped: each still embeds only the treasury id, so condition 4
    // passes, but neither is its role's contract
    YoloGenesis.treeIdsError(e.copy(treasuryAccumulationTreeHex = e.treasuryFundTreeHex,
      treasuryFundTreeHex = e.treasuryAccumulationTreeHex)) shouldBe None
    refused(x => x.copy(treasuryAccumulationTreeHex = x.treasuryFundTreeHex,
      treasuryFundTreeHex = x.treasuryAccumulationTreeHex),
      "treasuryAccumulationTreeHex is not the pinned accumulation contract",
      "treasuryFundTreeHex is not the pinned treasury fund contract", "template hash")
    refused(x => x.copy(lpAccumulationTreeHex = x.lpFundTreeHex, lpFundTreeHex = x.lpAccumulationTreeHex),
      "lpAccumulationTreeHex is not the pinned accumulation contract", "lpFundTreeHex is not the pinned LP fund contract")

    // an anyone-can-spend tree that merely contains the id: sigmaProp(id.size == 32)
    val anyoneCanSpend = "10020e20" + e.treasuryNftId + "0440d193b173007301"
    YoloEmissionSettings.parseTree(anyoneCanSpend).isSuccess shouldBe true
    YoloGenesis.treeIdsError(e.copy(treasuryAccumulationTreeHex = anyoneCanSpend)) shouldBe None
    refused(_.copy(treasuryAccumulationTreeHex = anyoneCanSpend),
      "treasuryAccumulationTreeHex is not the pinned accumulation contract", "template hash")
    refused(_.copy(treasuryFundTreeHex = anyoneCanSpend), "treasuryFundTreeHex is not the pinned treasury fund contract")
    refused(_.copy(emissionTreeHex = "10020e20" + e.emissionNftId + "0440d193b173007301"),
      "emissionTreeHex is not the pinned emission contract")

    // one non-key constant changed: the treasury fund floor (constant 22, Long 100 YOLO), an LP fund Int 720
    // constant (24; signWindow is 720) set back to 10, and the emission tree's tail reward (constant 9, 4.6875 YOLO)
    val treasuryFloor = withConstant(e.treasuryFundTreeHex, 22, sigma.ast.LongConstant(99000000000L))
    refused(_.copy(treasuryFundTreeHex = treasuryFloor),
      "treasuryFundTreeHex is not the pinned treasury fund contract", "constant 22 is", "expected 0580a0b787e905")
    val lpConst = withConstant(e.lpFundTreeHex, 24, sigma.ast.IntConstant(10))
    refused(_.copy(lpFundTreeHex = lpConst), "lpFundTreeHex is not the pinned LP fund contract",
      "constant 24 is 0414, expected 04a00b")
    YoloTreePins.Emission.constants(9) shouldBe "05c0cbacf622"
    val emissionFloor = withConstant(e.emissionTreeHex, 9, sigma.ast.LongConstant(1L))
    refused(_.copy(emissionTreeHex = emissionFloor), "emissionTreeHex is not the pinned emission contract",
      "constant 9 is", "expected 05c0cbacf622")
    // an accumulation tree with another id at the id index: the id is checked by value
    refused(x => x.copy(lpAccumulationTreeHex = withConstant(x.lpAccumulationTreeHex, 3,
      sigma.ast.ByteArrayConstant(Array.fill(32)(7: Byte)))), "lpAccumulationTreeHex")

    // other valid, distinct keys are accepted: the keys are the fund trees' free parameters
    val otherKeys = Seq(11, 12, 13).zip(Seq(101, 102, 103)).foldLeft(e.treasuryFundTreeHex) {
      case (hex, (i, k)) => withConstant(hex, i, key(k))
    }
    otherKeys should not be e.treasuryFundTreeHex
    YoloGenesis.treePinsError(e.copy(treasuryFundTreeHex = otherKeys)) shouldBe None
    val otherLpKeys = Seq(12, 13, 14).zip(Seq(201, 202, 203)).foldLeft(e.lpFundTreeHex) {
      case (hex, (i, k)) => withConstant(hex, i, key(k))
    }
    YoloGenesis.treePinsError(e.copy(lpFundTreeHex = otherLpKeys)) shouldBe None
    // and pass the whole launch check, once the digest is set for the new fund box
    YoloGenesis.launchError(withOwnDigest(withEmission(_.copy(treasuryFundTreeHex = otherKeys,
      lpFundTreeHex = otherLpKeys))).chainSettings) shouldBe None
    // the three keys must be pairwise distinct P2PK keys
    val twoEqual = withConstant(withConstant(e.lpFundTreeHex, 12, key(5)), 14, key(5))
    refused(_.copy(lpFundTreeHex = twoEqual), "lpFundTreeHex", "not pairwise distinct")
    val notAKey = withConstant(e.treasuryFundTreeHex, 12, sigma.ast.SigmaPropConstant(sigma.data.TrivialProp.TrueProp)
      .asInstanceOf[sigma.ast.Constant[sigma.ast.SType]])
    refused(_.copy(treasuryFundTreeHex = notAKey), "treasuryFundTreeHex", "constant 12 is not a P2PK key")
  }

  property("launch check 4c: the three test signer keys are the contract sources' keys and the devnet trees' keys") {
    // the keys as the contract sources write them, P2PK addresses with the Ergo mainnet prefix
    val sourceAddresses = Seq(
      "9gzkoMXatUr5s7jMBvjR7hzJyPqAyvZtdYxHZ9giJUEyvZ9nJde",
      "9fZM68VSqtjH3HibZngnQ9sgZXudKBt146mkKHWuur3qV4DKDYk",
      "9eYPis3GAjApr8RQKADhk4ZDaNQC7cM46pJi91jzJDa2RoymEzh")
    val encoder = new org.ergoplatform.ErgoAddressEncoder(org.ergoplatform.ErgoAddressEncoder.MainnetNetworkPrefix)
    sourceAddresses.map { a =>
      Base16.encode(encoder.fromString(a).get.asInstanceOf[org.ergoplatform.P2PKAddress].pubkeyBytes)
    } shouldBe YoloTreePins.TestSignerKeys
    // the devnet fund trees hold exactly these three, at the key indices
    YoloTreePins.testSignerKeyIndices(YoloTreePins.TreasuryFund, e.treasuryFundTree) shouldBe Seq(11, 12, 13)
    YoloTreePins.testSignerKeyIndices(YoloTreePins.LpFund, e.lpFundTree) shouldBe Seq(12, 13, 14)
    // and the devnet config is the one that allows them
    e.allowTestSignerKeys shouldBe true
    YoloGenesis.testSignerKeysError(e) shouldBe None
  }

  property("launch check 4c: with allowTestSignerKeys = false a fund tree holding a test signer key is refused") {
    // the devnet trees as they are
    refused(_.copy(allowTestSignerKeys = false), "treasuryFundTreeHex holds a public test signer key at constant(s) 11, 12, 13",
      "lpFundTreeHex holds a public test signer key at constant(s) 12, 13, 14", "allowTestSignerKeys = true",
      "must never guard real funds")

    val ownTreasury = Seq(11, 12, 13).zip(Seq(101, 102, 103)).foldLeft(e.treasuryFundTreeHex) {
      case (hex, (i, k)) => withConstant(hex, i, key(k))
    }
    val ownLp = Seq(12, 13, 14).zip(Seq(201, 202, 203)).foldLeft(e.lpFundTreeHex) {
      case (hex, (i, k)) => withConstant(hex, i, key(k))
    }
    // one fund with its own keys is not enough: the other still holds the test keys
    refused(_.copy(allowTestSignerKeys = false, treasuryFundTreeHex = ownTreasury), "lpFundTreeHex holds a public " +
      "test signer key at constant(s) 12, 13, 14")
    launchError(_.copy(allowTestSignerKeys = false, treasuryFundTreeHex = ownTreasury)).get should not include (
      "treasuryFundTreeHex holds")
    // a single test key among own keys is refused, and named by its constant index
    val oneTestKey = withConstant(ownLp, 13, e.lpFundTree.constants(13))
    YoloTreePins.isTestSignerKey(e.lpFundTree.constants(13)) shouldBe true
    YoloGenesis.treePinsError(e.copy(lpFundTreeHex = oneTestKey)) shouldBe None
    refused(_.copy(allowTestSignerKeys = false, treasuryFundTreeHex = ownTreasury, lpFundTreeHex = oneTestKey),
      "lpFundTreeHex holds a public test signer key at constant(s) 13")

    // both funds with their own keys: accepted with the setting off, and the whole launch check passes once the
    // digest is set for the new fund boxes
    YoloGenesis.testSignerKeysError(e.copy(allowTestSignerKeys = false, treasuryFundTreeHex = ownTreasury,
      lpFundTreeHex = ownLp)) shouldBe None
    YoloGenesis.launchError(withOwnDigest(withEmission(_.copy(allowTestSignerKeys = false,
      treasuryFundTreeHex = ownTreasury, lpFundTreeHex = ownLp))).chainSettings) shouldBe None
    // the setting does not refuse own keys either way
    YoloGenesis.testSignerKeysError(e.copy(allowTestSignerKeys = true, treasuryFundTreeHex = ownTreasury,
      lpFundTreeHex = ownLp)) shouldBe None
  }

  property("launch check 4c: with allowTestSignerKeys = true, a start-up warning names each fund that holds a test key") {
    val ownTreasury = Seq(11, 12, 13).zip(Seq(101, 102, 103)).foldLeft(e.treasuryFundTreeHex) {
      case (hex, (i, k)) => withConstant(hex, i, key(k))
    }
    val ownLp = Seq(12, 13, 14).zip(Seq(201, 202, 203)).foldLeft(e.lpFundTreeHex) {
      case (hex, (i, k)) => withConstant(hex, i, key(k))
    }
    val guarded = "guarded by public test signer keys, whose secrets are public"
    val never = "This must never be a public network"

    // the devnet trees as they are: both funds named, and the launch check still passes
    val w = YoloGenesis.testSignerKeysWarning(e)
    w shouldBe defined
    w.get should include("allowTestSignerKeys = true")
    w.get should include("the treasury fund (treasuryFundTreeHex, constant(s) 11, 12, 13)")
    w.get should include("the LP fund (lpFundTreeHex, constant(s) 12, 13, 14)")
    w.get should include(s"are $guarded")
    w.get should include(never)
    YoloGenesis.launchError(devnet.chainSettings) shouldBe None

    // one fund with its own keys: only the other is named
    val lpOnly = YoloGenesis.testSignerKeysWarning(e.copy(treasuryFundTreeHex = ownTreasury))
    lpOnly shouldBe defined
    lpOnly.get should include("the LP fund (lpFundTreeHex, constant(s) 12, 13, 14)")
    lpOnly.get should not include ("treasury fund")
    lpOnly.get should include(s"is $guarded")
    lpOnly.get should include(never)

    // both funds with their own keys, or the setting false: no warning
    YoloGenesis.testSignerKeysWarning(e.copy(treasuryFundTreeHex = ownTreasury, lpFundTreeHex = ownLp)) shouldBe None
    YoloGenesis.testSignerKeysWarning(e.copy(allowTestSignerKeys = false)) shouldBe None
  }

  property("launch check 5: the built emission box has exactly tokens(0) = (id, 1) and R4, R5 = accumulation hashes") {
    val good = YoloGenesis.emissionBox(e)
    YoloGenesis.emissionBoxError(e, good) shouldBe None
    val id = e.emissionNftIdBytes
    val r4 = ByteArrayConstant(Base16.decode(TreasuryAccumulationHash).get)
    val r5 = ByteArrayConstant(Base16.decode(LpAccumulationHash).get)
    def box(tokens: Seq[(ErgoBox.TokenId, Long)] = Seq(token(id)),
            regs: ErgoBox.AdditionalRegisters = Map(R4 -> r4, R5 -> r5),
            value: Long = e.genesisValue,
            tree: sigma.ast.ErgoTree = e.emissionTree): ErgoBox =
      ErgoState.createGenesisBox(value, tree, tokens, regs)
    box() shouldBe good
    val mutations: Seq[(String, ErgoBox, String)] = Seq(
      ("no token", box(tokens = Seq.empty), "exactly one token"),
      ("amount 2", box(tokens = Seq(token(id, 2L))), "exactly one token"),
      ("second token", box(tokens = Seq(token(id), token(e.lpNftIdBytes))), "exactly one token"),
      ("other token", box(tokens = Seq(token(e.treasuryNftIdBytes))), "exactly one token"),
      ("extra R6", box(regs = Map(R4 -> r4, R5 -> r5, R6 -> ByteArrayConstant(Zero32))), "R4, R5 only"),
      ("no R5", box(regs = Map(R4 -> r4)), "R4, R5 only"),
      ("R4 and R5 swapped", box(regs = Map(R4 -> r5, R5 -> r4)), "R4 must be"),
      ("R4 a fund tree hash", box(regs = Map(R4 -> ByteArrayConstant(YoloGenesis.treeHash(e.treasuryFundTree)), R5 -> r5)),
        "R4 must be"),
      ("R5 Long", box(regs = Map(R4 -> r4, R5 -> LongConstant(0L))), "R5 must be"),
      ("other value", box(value = 1L), "is not genesisValue"),
      ("other tree", box(tree = e.lpFundTree), "not at the emission tree")
    )
    mutations.foreach { case (name, b, msg) =>
      withClue(s"$name: ") {
        val err = YoloGenesis.emissionBoxError(e, b)
        err shouldBe defined
        err.get should include("genesis emission box is wrong")
        err.get should include(msg)
      }
    }
  }

  property("launch check 6: fund box register layouts, whitelist hashes 32 bytes, heights 0, at most 64 entries") {
    val treasury = YoloGenesis.treasuryFundBox(e)
    val lp = YoloGenesis.lpFundBox(e)
    YoloGenesis.fundBoxesError(e, treasury, lp) shouldBe None

    // config mutations, refused through the built LP box
    val h32 = "ab" * 32
    refused(_.copy(lpWhitelist = Seq(YoloWhitelistEntry("ab" * 31, 0L))), "lpWhitelist(0).hash is 31 bytes")
    refused(_.copy(lpWhitelist = Seq(YoloWhitelistEntry(h32, 0L), YoloWhitelistEntry("ab" * 33, 0L))),
      "lpWhitelist(1).hash is 33 bytes")
    refused(_.copy(lpWhitelist = Seq(YoloWhitelistEntry(h32, 5L))), "lpWhitelist(0).height is 5")
    refused(_.copy(lpWhitelist = Seq(YoloWhitelistEntry(h32, -1L))), "lpWhitelist(0).height is -1")
    refused(_.copy(lpWhitelist = Seq.fill(65)(YoloWhitelistEntry(h32, 0L))), "65 entries, at most 64")
    // 64 entries of 32-byte hashes at height 0 pass
    YoloGenesis.launchError(withOwnDigest(withEmission(_.copy(lpWhitelist = (0 until 64).map(i =>
      YoloWhitelistEntry(f"$i%064x", 0L))))).chainSettings) shouldBe None

    // box mutations
    def treasuryBox(regs: ErgoBox.AdditionalRegisters,
                    tokens: Seq[(ErgoBox.TokenId, Long)] = Seq(token(e.treasuryNftIdBytes))): ErgoBox =
      ErgoState.createGenesisBox(e.treasuryFundValue, e.treasuryFundTree, tokens, regs)
    def lpBox(regs: ErgoBox.AdditionalRegisters,
              tokens: Seq[(ErgoBox.TokenId, Long)] = Seq(token(e.lpNftIdBytes))): ErgoBox =
      ErgoState.createGenesisBox(e.lpFundValue, e.lpFundTree, tokens, regs)
    val tRegs: ErgoBox.AdditionalRegisters = Map(R4 -> ByteArrayConstant(Zero32), R5 -> LongConstant(0L),
      R6 -> ByteArrayConstant(Array.emptyByteArray), R7 -> LongConstant(0L), R8 -> LongConstant(0L))
    val emptyWl = YoloGenesis.whitelistConstant(Seq.empty)
    val lRegs: ErgoBox.AdditionalRegisters = Map(R4 -> emptyWl, R5 -> LongConstant(0L), R6 -> LongConstant(0L),
      R7 -> ByteArrayConstant(Zero32))
    treasuryBox(tRegs) shouldBe treasury
    lpBox(lRegs) shouldBe lp

    val treasuryMutations: Seq[(String, ErgoBox, String)] = Seq(
      ("R5 Int", treasuryBox(tRegs.updated(R5, IntConstant(0))), "R5 must be"),
      ("R7 1", treasuryBox(tRegs.updated(R7, LongConstant(1L))), "R7 must be"),
      ("R6 not empty", treasuryBox(tRegs.updated(R6, ByteArrayConstant(Zero32))), "R6 must be"),
      ("R4 31 bytes", treasuryBox(tRegs.updated(R4, ByteArrayConstant(Array.fill(31)(0: Byte)))), "R4 must be"),
      ("no R8", treasuryBox(tRegs - R8), "R4, R5, R6, R7, R8 only"),
      ("extra R9", treasuryBox(tRegs.updated(R9, LongConstant(0L))), "R4, R5, R6, R7, R8 only"),
      ("no token", treasuryBox(tRegs, Seq.empty), "exactly one token"),
      ("LP token", treasuryBox(tRegs, Seq(token(e.lpNftIdBytes))), "exactly one token")
    )
    treasuryMutations.foreach { case (name, b, msg) =>
      withClue(s"treasury, $name: ") {
        val err = YoloGenesis.fundBoxesError(e, b, lp)
        err shouldBe defined
        err.get should include("treasury fund box")
        err.get should include(msg)
      }
    }

    val wrongPairs = CollectionConstant[SCollection[SByte.type]](
      Colls.fromItems(Colls.fromArray(Zero32)), SCollection(SByte))
    val lpMutations: Seq[(String, ErgoBox, String)] = Seq(
      ("R4 Coll[Coll[Byte]]", lpBox(lRegs.updated(R4, wrongPairs)), "R4 must be"),
      ("R4 Coll[Byte]", lpBox(lRegs.updated(R4, ByteArrayConstant(Array.emptyByteArray))), "R4 must be"),
      ("R5 Int", lpBox(lRegs.updated(R5, IntConstant(0))), "R5 must be Long 0"),
      ("R6 1", lpBox(lRegs.updated(R6, LongConstant(1L))), "R6 must be Long 0"),
      ("R7 empty", lpBox(lRegs.updated(R7, ByteArrayConstant(Array.emptyByteArray))), "R7 must be"),
      ("extra R8", lpBox(lRegs.updated(R8, LongConstant(0L))), "R4, R5, R6, R7 only"),
      ("token amount 2", lpBox(lRegs, Seq(token(e.lpNftIdBytes, 2L))), "exactly one token"),
      ("whitelist not as configured", lpBox(lRegs.updated(R4, YoloGenesis.whitelistConstant(Seq(Zero32 -> 0L)))),
        "not the configured lpWhitelist")
    )
    lpMutations.foreach { case (name, b, msg) =>
      withClue(s"LP, $name: ") {
        val err = YoloGenesis.fundBoxesError(e, treasury, b)
        err shouldBe defined
        err.get should include("LP fund box")
        err.get should include(msg)
      }
    }
  }

  property("launch check 7: each fund box at least 100 YOLO, genesis emission value positive and not stranding the box") {
    YoloGenesis.FundFloor shouldBe 100000000000L
    YoloGenesis.valuesError(e) shouldBe None
    launchError(_.copy(treasuryFundValue = YoloGenesis.FundFloor, lpFundValue = YoloGenesis.FundFloor)) shouldBe None
    refused(_.copy(treasuryFundValue = YoloGenesis.FundFloor - 1), "treasuryFundValue = 99999999999")
    refused(_.copy(lpFundValue = YoloGenesis.FundFloor - 1), "lpFundValue = 99999999999")
    refused(_.copy(lpFundValue = 0L), "lpFundValue = 0")
    refused(_.copy(genesisValue = 0L), "genesisValue = 0")
    refused(_.copy(genesisValue = -1L), "genesisValue = -1")

    // the genesis value must not strand the emission box: the last box and the terminal outputs each hold at least
    // MinValueMax (10,000) nanocoins per byte. Executed counter-examples, refused:
    org.ergoplatform.settings.Parameters.MinValueMax shouldBe 10000
    // a last box of 1 nano at 26,258,984
    refused(_.copy(genesisValue = 177291450000000001L), "strands the emission box", "spent at height 26258984",
      "holds v = 1;", "the last emission box holds 1, below", "MinValueMax 10000 x")
    // a last box of 0.001 YOLO
    refused(_.copy(genesisValue = 177291450001000000L), "strands the emission box", "holds v = 1000000;",
      "the last emission box holds 1000000, below", "the terminal treasury share holds 40000, below")
    // a 1 nano genesis box: terminal at height 1, every share below the bound
    refused(_.copy(genesisValue = 1L), "strands the emission box", "spent at height 1", "holds v = 1;",
      "the terminal treasury share holds 0", "the terminal LP share holds 0", "the terminal miner output holds 1")
    // 37.5 YOLO + 1 nano: a 1 nano box at height 2
    refused(_.copy(genesisValue = 37500000001L), "strands the emission box", "spent at height 2", "holds v = 1;")
    // the devnet value (last box 1.5625 YOLO) and the schedule supply (last box 4.6875 YOLO) do not strand the box
    YoloGenesis.strandedEmissionError(e) shouldBe None
    e.genesisValue shouldBe 177291250000000000L
    YoloGenesis.strandedEmissionError(e.copy(genesisValue = 177291450000000000L)) shouldBe None
  }

  property("launch check 8: genesisStateDigestHex must be the digest of the YOLO genesis boxes") {
    val computed = Base16.encode(YoloGenesis.stateDigest(ErgoState.genesisBoxes(devnet.chainSettings)))
    computed shouldBe devnet.chainSettings.genesisStateDigestHex
    YoloGenesis.digestError(devnet.chainSettings, ErgoState.genesisBoxes(devnet.chainSettings)) shouldBe None
    // the stale digest of the earlier 177,291,450 YOLO devnet genesis: refused, the computed value printed
    val stale = devnet.chainSettings.copy(
      genesisStateDigestHex = "4915ac03b4350bd727dc01017e3f179d28e51a443b875f71f2e3d3390a1c427a03")
    val err = YoloGenesis.launchError(stale)
    err shouldBe defined
    err.get should include("genesisStateDigestHex = 4915ac03b4350bd727dc01017e3f179d28e51a443b875f71f2e3d3390a1c427a03")
    err.get should include(s"""computed: genesisStateDigestHex = "$computed"""")
    // the digest of the v1.4 devnet genesis (the emission tree before the v1.5 schedule): refused
    computed should not be "650a702c5d1d56d38063c64de26b568c63cb99eb8b337d0058decfb7162814df03"
    YoloGenesis.launchError(devnet.chainSettings.copy(
      genesisStateDigestHex = "650a702c5d1d56d38063c64de26b568c63cb99eb8b337d0058decfb7162814df03")).get should
      include(s"""computed: genesisStateDigestHex = "$computed"""")
    // a changed genesis box (another genesis value) with the devnet digest: refused
    refused(_.copy(genesisValue = 177291450000000000L), "is not the digest of the YOLO genesis boxes")
  }

  property("launch check with the switch off: nothing is checked") {
    YoloGenesis.launchError(settings.chainSettings) shouldBe None
    YoloGenesis.launchError(devnet.chainSettings.copy(yolo = devnet.chainSettings.yolo.copy(enabled = false,
      emission = YoloEmissionSettings()))) shouldBe None
  }

  // ------------------------------------------------------------------ devnet digest and switch off

  property("yolo-devnet.conf genesisStateDigestHex equals the digest of the devnet genesis boxes") {
    // the only genesis input not in yolo-devnet.conf is the no-premine proof list: make sure it is the main
    // application.conf one a node reads, not a test override
    val mainAppConf = getClass.getClassLoader.getResources("application.conf").asScala.toSeq
      .find(u => !u.toString.contains("test-classes")).get
    devnet.chainSettings.noPremineProof shouldBe
      ConfigFactory.parseURL(mainAppConf).getStringList("ergo.chain.noPremineProof").asScala.toSeq
    val computed = digestOf(ErgoState.genesisBoxes(devnet.chainSettings))
    withClue(s"computed devnet genesis state digest $computed: ") {
      devnet.chainSettings.genesisStateDigestHex shouldBe computed
    }
    // the node's own genesis path asserts the same and records the emission box
    val (us, bh) = ErgoState.generateGenesisUtxoState(createTempDir, devnet)
    Base16.encode(us.rootDigest) shouldBe computed
    bh.boxes.size shouldBe 4
    us.emissionBoxOpt shouldBe Some(YoloGenesis.emissionBox(e))
    us.genesisBoxes shouldBe ErgoState.genesisBoxes(devnet.chainSettings)
    us.closeStorage()
  }

  property("switch off: upstream genesis boxes and digests are unchanged") {
    // upstream test settings
    val up = ErgoState.genesisBoxes(settings.chainSettings)
    up.size shouldBe 3
    up.head.ergoTree shouldBe settings.chainSettings.monetary.emissionBoxProposition
    up.head.value shouldBe settings.chainSettings.emissionRules.minersCoinsTotal
    up.head.additionalTokens.length shouldBe 0
    up.head.additionalRegisters shouldBe empty
    up(1) shouldBe ErgoState.noPremineBox(settings.chainSettings)
    up(2).ergoTree shouldBe org.ergoplatform.ErgoTreePredef.foundationScript(settings.chainSettings.monetary)
    digestOf(up) shouldBe settings.chainSettings.genesisStateDigestHex

    // the shipped Ergo testnet and mainnet confs over the main application.conf (read from the files, as the test
    // classpath puts the test application.conf, with test founders keys, first): their configured digests
    Seq("src/main/resources/testnet.conf" -> "cb63aa99a3060f341781d8662b58bf18b9ad258db4fe88d09f8f71cb668cad4502",
      "src/main/resources/mainnet.conf" -> "a5df145d41ab15a01e0cd3ffbab046f0d029e5412293072ad0f5827428589b9302")
      .foreach { case (conf, digest) =>
        withClue(s"$conf: ") {
          val cs = ChainSettingsReader.read(conf).get
          cs.yolo.enabled shouldBe false
          cs.genesisStateDigestHex shouldBe digest
          val offBoxes = ErgoState.genesisBoxes(cs)
          offBoxes.size shouldBe 3
          offBoxes.head.ergoTree shouldBe cs.monetary.emissionBoxProposition
          digestOf(offBoxes) shouldBe digest
          // and the YOLO emission rules are not reachable
          cs.yoloEmissionRulesOpt shouldBe None
        }
      }

    // the devnet chain settings with the switch off give the upstream boxes for those settings
    val off = devnet.chainSettings.copy(yolo = YoloSettings.disabled)
    ErgoState.genesisBoxes(off).size shouldBe 3
    ErgoState.genesisBoxes(off).head.ergoTree shouldBe off.monetary.emissionBoxProposition
  }

  property("the YOLO emission rules are reachable from the devnet chain settings") {
    val rules = devnet.chainSettings.yoloEmissionRulesOpt.get
    rules.genesisValue shouldBe 177291250000000000L
    rules.terminalHeight shouldBe 26258941L
    YoloEmissionRules.rewardAtHeight(1) shouldBe 37500000000L
    devnet.launchParameters shouldBe YoloLaunchParameters
  }

}
