package org.ergoplatform.nodeView.state

import org.ergoplatform.{ErgoBox, ErgoBoxCandidate}
import org.ergoplatform.ErgoBox.{NonMandatoryRegisterId, R4, R5, R6, R7, R8}
import org.ergoplatform.mining.YoloEmissionTx
import org.ergoplatform.mining.emission.YoloEmissionRules
import org.ergoplatform.settings.Algos.HF
import org.ergoplatform.settings.{ChainSettings, Parameters, YoloEmissionSettings}
import scorex.crypto.authds.{ADDigest, ADValue}
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, Insert}
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.util.bytesToId
import scorex.util.encode.Base16
import sigma.{Coll, Colls}
import sigma.ast.{ByteArrayConstant, CollectionConstant, Constant, ErgoTree, EvaluatedValue, LongConstant, SByte, SCollection,
  SLong, STuple, SType}
import sigma.crypto.CryptoConstants
import sigma.data.{Digest32Coll, ProveDlog, RType}

import scala.util.{Failure, Success, Try}

/**
  * YOLO (SigmaChain) genesis state and its launch check.
  *
  * With `ergo.chain.yolo.enabled = true`, the genesis state is, in this order:
  *  1. the emission box: emission tree, `tokens(0)` = (emission NFT, 1), R4 = blake2b256(treasury accumulation
  *     tree), R5 = blake2b256(LP accumulation tree), no other register. First, so it is recorded as the emission box;
  *  2. the treasury fund box: treasury fund tree, `tokens(0)` = (treasury NFT, 1), R4 `Coll[Byte]` 32 zero bytes,
  *     R5 `Long` 0, R6 `Coll[Byte]` empty, R7 `Long` 0, R8 `Long` 0;
  *  3. the LP fund box: LP fund tree, `tokens(0)` = (LP NFT, 1), R4 `Coll[(Coll[Byte], Long)]` the configured
  *     whitelist, R5 `Long` 0, R6 `Long` 0, R7 `Coll[Byte]` 32 zero bytes;
  *  4. the no-premine box, built as upstream (whether to keep it is a launch decision, to be taken before mainnet;
  *     dropping it changes the genesis state digest).
  * There is no founders box. All boxes have creation height 0 as upstream; the NFTs are not minted, as genesis
  * boxes are inserted into the state directly.
  *
  * `launchError` is the start-up check, called by `ErgoSettingsReader` when the switch is on. Its ten conditions (1 to
  * 8, with 4b the contract identity pins, 4c the refusal of the public test signer keys, and 8 the genesis state
  * digest) are also exposed one by one, for tests.
  */
object YoloGenesis {

  /** Config path of the emission inputs, used in messages. */
  val ConfigPath: String = "ergo.chain.yolo.emission"

  /** Minimum value of each fund box: `fundFloor` in treasury_governance.es and lp_fund.es, 100 YOLO. */
  val FundFloor: Long = 100L * YoloEmissionRules.CoinsInOneYolo

  /** Maximum number of LP whitelist entries: `maxWhitelist` in lp_fund.es. */
  val MaxWhitelistEntries: Int = 64

  /** Height allowed for a genesis whitelist entry: genesis is height 0. */
  val GenesisWhitelistHeight: Long = 0L

  /** Register type of the LP whitelist (lp_fund.es R4): `Coll[(Coll[Byte], Long)]`. */
  val WhitelistType: SCollection[STuple] = SCollection(STuple(SCollection(SByte), SLong))

  private val ZeroHash: Array[Byte] = Array.fill(YoloEmissionSettings.IdLength)(0: Byte)

  private def tokenOf(id: Array[Byte]): Seq[(ErgoBox.TokenId, Long)] = Seq((Digest32Coll @@ Colls.fromArray(id)) -> 1L)

  /** blake2b256 of a tree's bytes: what emission.es compares an output's `propositionBytes` hash with. */
  def treeHash(tree: ErgoTree): Array[Byte] = Blake2b256(tree.bytes)

  /** The genesis emission box. Throws if the inputs are malformed (see `launchError`). */
  def emissionBox(e: YoloEmissionSettings): ErgoBox = ErgoState.createGenesisBox(
    e.genesisValue,
    e.emissionTree,
    tokenOf(e.emissionNftIdBytes),
    Map(
      R4 -> ByteArrayConstant(treeHash(e.treasuryAccumulationTree)),
      R5 -> ByteArrayConstant(treeHash(e.lpAccumulationTree))))

  /** The genesis treasury fund box. Throws if the inputs are malformed. */
  def treasuryFundBox(e: YoloEmissionSettings): ErgoBox = ErgoState.createGenesisBox(
    e.treasuryFundValue,
    e.treasuryFundTree,
    tokenOf(e.treasuryNftIdBytes),
    Map(
      R4 -> ByteArrayConstant(ZeroHash),
      R5 -> LongConstant(0L),
      R6 -> ByteArrayConstant(Array.emptyByteArray),
      R7 -> LongConstant(0L),
      R8 -> LongConstant(0L)))

  /**
    * LP whitelist as an `R4` register value of type `Coll[(Coll[Byte], Long)]`, also when empty.
    * Throws if a hash is not hex.
    */
  def whitelistConstant(entries: Seq[(Array[Byte], Long)]): Constant[SCollection[STuple]] = {
    implicit val itemType: RType[(Coll[Byte], Long)] = RType.pairRType(sigma.collRType(sigma.ByteType), sigma.LongType)
    val items: Array[(Coll[Byte], Long)] = entries.map { case (h, height) => (Colls.fromArray(h), height) }.toArray
    CollectionConstant[STuple](Colls.fromArray(items).asInstanceOf[Coll[STuple#WrappedType]], WhitelistType.elemType)
  }

  /** The genesis LP fund box. Throws if the inputs are malformed. */
  def lpFundBox(e: YoloEmissionSettings): ErgoBox = {
    val entries = e.lpWhitelist.map { w =>
      Base16.decode(w.hash).getOrElse(throw new IllegalArgumentException(s"whitelist hash `${w.hash}` is not hex")) ->
        w.height
    }
    ErgoState.createGenesisBox(
      e.lpFundValue,
      e.lpFundTree,
      tokenOf(e.lpNftIdBytes),
      Map(
        R4 -> whitelistConstant(entries),
        R5 -> LongConstant(0L),
        R6 -> LongConstant(0L),
        R7 -> ByteArrayConstant(ZeroHash)))
  }

  /**
    * YOLO genesis state boxes: emission, treasury fund, LP fund, then the no-premine box, built as upstream.
    * Throws if the inputs are malformed; the start-up check (`launchError`) refuses such a config first.
    */
  def genesisBoxes(chainSettings: ChainSettings): Seq[ErgoBox] = {
    val e = chainSettings.yolo.emission
    Seq(emissionBox(e), treasuryFundBox(e), lpFundBox(e), ErgoState.noPremineBox(chainSettings))
  }

  // ---------------------------------------------------------------- launch check

  private def fail(msg: String): Option[String] = Some(s"ergo.chain.yolo.enabled = true but $msg")

  private def ids(e: YoloEmissionSettings): Seq[(String, Array[Byte])] = Seq(
    "emissionNftId" -> e.emissionNftIdBytes,
    "treasuryNftId" -> e.treasuryNftIdBytes,
    "lpNftId" -> e.lpNftIdBytes)

  /**
    * Condition 1: all five trees parse as ErgoTrees and every hex and id field is well formed (ids exactly
    * 32 bytes, whitelist hashes hex). Names every missing or malformed field.
    */
  def inputsError(e: YoloEmissionSettings): Option[String] = {
    val trees = Seq(
      "emissionTreeHex" -> e.emissionTreeHex,
      "treasuryAccumulationTreeHex" -> e.treasuryAccumulationTreeHex,
      "lpAccumulationTreeHex" -> e.lpAccumulationTreeHex,
      "treasuryFundTreeHex" -> e.treasuryFundTreeHex,
      "lpFundTreeHex" -> e.lpFundTreeHex)
    val treeErrors = trees.flatMap { case (name, hex) =>
      YoloEmissionSettings.parseTree(hex) match {
        case Success(_) => None
        case Failure(t) => Some(s"$name: ${if (hex.isEmpty) "unset" else s"not an ErgoTree (${t.getMessage})"}")
      }
    }
    val idErrors = Seq("emissionNftId" -> e.emissionNftId, "treasuryNftId" -> e.treasuryNftId, "lpNftId" -> e.lpNftId)
      .flatMap { case (name, hex) =>
        YoloEmissionSettings.decodeId(hex) match {
          case Success(_) => None
          case Failure(t) => Some(s"$name: ${if (hex.isEmpty) "unset" else s"not a 32-byte hex id (${t.getMessage})"}")
        }
      }
    val whitelistErrors = e.lpWhitelist.zipWithIndex.flatMap { case (w, i) =>
      if (Base16.decode(w.hash).isSuccess && w.hash.nonEmpty) None else Some(s"lpWhitelist($i).hash: `${w.hash}` is not hex")
    }
    val all = treeErrors ++ idErrors ++ whitelistErrors
    if (all.isEmpty) None else fail(s"$ConfigPath is missing or malformed: ${all.mkString("; ")}")
  }

  /**
    * Condition 2: the three NFT ids are pairwise distinct and none has all 32 bytes equal (the test placeholders
    * `cc`, `aa`, `bb` repeated). Needs condition 1.
    */
  def idsError(e: YoloEmissionSettings): Option[String] = {
    val named = ids(e)
    val placeholders = named.collect { case (name, id) if id.forall(_ == id.head) => s"$name (${Base16.encode(id)})" }
    val duplicates = for {
      i <- named.indices
      j <- named.indices if i < j
      if java.util.Arrays.equals(named(i)._2, named(j)._2)
    } yield s"${named(i)._1} = ${named(j)._1}"
    if (placeholders.nonEmpty) {
      fail(s"$ConfigPath has a placeholder NFT id (all bytes equal): ${placeholders.mkString(", ")}")
    } else if (duplicates.nonEmpty) {
      fail(s"$ConfigPath NFT ids must be distinct: ${duplicates.mkString(", ")}")
    } else {
      None
    }
  }

  /**
    * Condition 3: no NFT id equals the id of a genesis box. A transaction mints a token whose id equals the id of
    * its first input, so the first spender of such a box could mint a second unit of the NFT.
    */
  def boxIdCollisionError(e: YoloEmissionSettings, boxes: Seq[ErgoBox]): Option[String] = {
    val hits = for {
      (name, id) <- ids(e)
      (box, i) <- boxes.zipWithIndex
      if java.util.Arrays.equals(box.id, id)
    } yield s"$name equals the id of genesis box $i"
    if (hits.isEmpty) None else fail(s"$ConfigPath NFT id equals a genesis box id: ${hits.mkString(", ")}")
  }

  /** 32-byte `Coll[Byte]` constants of a tree (segregated constants only). */
  private def idConstants(tree: ErgoTree): Seq[Array[Byte]] = tree.constants.collect {
    case ByteArrayConstant(bytes) if bytes.length == YoloEmissionSettings.IdLength => bytes.toArray
  }

  /**
    * Condition 4: the emission tree embeds the emission NFT id; the treasury accumulation and treasury fund trees
    * embed the treasury id (the one the treasury fund box holds at `tokens(0)`); the LP accumulation and LP fund
    * trees embed the LP id; no tree embeds either of the other two ids. Ids are looked for among the tree's
    * segregated `Coll[Byte]` constants. The five trees must also be pairwise different. Needs condition 1.
    */
  def treeIdsError(e: YoloEmissionSettings): Option[String] = {
    val emissionId = e.emissionNftIdBytes
    val treasuryId = e.treasuryNftIdBytes
    val lpId = e.lpNftIdBytes
    val all = Seq("emission" -> emissionId, "treasury" -> treasuryId, "LP" -> lpId)
    val expected = Seq(
      ("emissionTreeHex", e.emissionTree, "emission"),
      ("treasuryAccumulationTreeHex", e.treasuryAccumulationTree, "treasury"),
      ("treasuryFundTreeHex", e.treasuryFundTree, "treasury"),
      ("lpAccumulationTreeHex", e.lpAccumulationTree, "LP"),
      ("lpFundTreeHex", e.lpFundTree, "LP"))
    val errors = expected.flatMap { case (name, tree, own) =>
      val consts = idConstants(tree)
      def embeds(id: Array[Byte]): Boolean = consts.exists(java.util.Arrays.equals(_, id))
      val ownId = all.find(_._1 == own).get._2
      val others = all.filter(_._1 != own).collect { case (n, id) if embeds(id) => n }
      if (others.nonEmpty) {
        Some(s"$name embeds the ${others.mkString(" and ")} NFT id (it must embed only the $own NFT id)")
      } else if (!embeds(ownId)) {
        Some(s"$name does not embed the $own NFT id as a constant")
      } else {
        None
      }
    }
    val treeBytes = expected.map { case (name, tree, _) => name -> tree.bytes }
    val sameTrees = for {
      i <- treeBytes.indices
      j <- treeBytes.indices if i < j
      if java.util.Arrays.equals(treeBytes(i)._2, treeBytes(j)._2)
    } yield s"${treeBytes(i)._1} = ${treeBytes(j)._1}"
    val allErrors = errors ++ sameTrees.map(s => s"the same tree twice: $s")
    if (allErrors.isEmpty) None else fail(s"$ConfigPath trees and NFT ids do not match: ${allErrors.mkString("; ")}")
  }

  /**
    * Condition 4b: each tree is the pinned contract of its role (`YoloTreePins`): header byte, template hash, number
    * of constants and the value of every constant, with the role's own NFT id at the pinned index and, for the two
    * fund trees, three pairwise distinct P2PK signer keys as the only other free constants. A tree that merely embeds
    * the right id, or the accumulation and fund trees of one fund swapped, is refused. Needs condition 1.
    */
  def treePinsError(e: YoloEmissionSettings): Option[String] = {
    val expected = Seq(
      ("emissionTreeHex", e.emissionTree, YoloTreePins.Emission, e.emissionNftIdBytes, "emission"),
      ("treasuryAccumulationTreeHex", e.treasuryAccumulationTree, YoloTreePins.Accumulation, e.treasuryNftIdBytes,
        "treasury"),
      ("treasuryFundTreeHex", e.treasuryFundTree, YoloTreePins.TreasuryFund, e.treasuryNftIdBytes, "treasury"),
      ("lpAccumulationTreeHex", e.lpAccumulationTree, YoloTreePins.Accumulation, e.lpNftIdBytes, "LP"),
      ("lpFundTreeHex", e.lpFundTree, YoloTreePins.LpFund, e.lpNftIdBytes, "LP"))
    val errors = expected.flatMap { case (name, tree, pin, id, idName) =>
      val m = YoloTreePins.mismatches(pin, tree, id, idName)
      if (m.isEmpty) None else Some(s"$name is not the pinned ${pin.role} contract: ${m.mkString(", ")}")
    }
    if (errors.isEmpty) None else fail(s"$ConfigPath trees are not the YOLO contracts: ${errors.mkString("; ")}")
  }

  /**
    * Condition 4c: unless `allowTestSignerKeys` is true (a local devnet only), neither fund tree holds any of the
    * three public test signer keys of the contract sources (`YoloTreePins.TestSignerKeys`) among its signer keys.
    * Needs condition 4b.
    */
  def testSignerKeysError(e: YoloEmissionSettings): Option[String] = {
    if (e.allowTestSignerKeys) {
      None
    } else {
      val errors = testSignerKeyHits(e).map { case (name, _, hits) =>
        s"$name holds a public test signer key at constant(s) ${hits.mkString(", ")}"
      }
      if (errors.isEmpty) {
        None
      } else {
        fail(s"${errors.mkString("; ")}. These keys come from the contract sources and must never guard real funds; " +
          s"compile the fund contracts with the network's own signer keys, or set $ConfigPath.allowTestSignerKeys = true " +
          "on a local devnet only")
      }
    }
  }

  /**
    * The fund trees that hold one of the public test signer keys: (config key, fund, constant indices). Needs
    * condition 4b.
    */
  private def testSignerKeyHits(e: YoloEmissionSettings): Seq[(String, String, Seq[Int])] = Seq(
    ("treasuryFundTreeHex", "treasury fund", e.treasuryFundTree, YoloTreePins.TreasuryFund),
    ("lpFundTreeHex", "LP fund", e.lpFundTree, YoloTreePins.LpFund)
  ).flatMap { case (name, fund, tree, pin) =>
    val hits = YoloTreePins.testSignerKeyIndices(pin, tree)
    if (hits.isEmpty) None else Some((name, fund, hits))
  }

  /**
    * The start-up warning that goes with condition 4c: with `allowTestSignerKeys = true`, the funds whose trees do
    * hold a public test signer key, named, or None if neither does or the setting is false. It does not change the
    * launch check's result; `ErgoSettingsReader` logs it at ERROR level at every start, once the launch check has
    * passed. Needs condition 4b.
    */
  def testSignerKeysWarning(e: YoloEmissionSettings): Option[String] = {
    if (!e.allowTestSignerKeys) {
      None
    } else {
      val funds = testSignerKeyHits(e).map { case (name, fund, hits) =>
        s"the $fund ($name, constant(s) ${hits.mkString(", ")})"
      }
      if (funds.isEmpty) {
        None
      } else {
        Some(s"$ConfigPath.allowTestSignerKeys = true and ${funds.mkString(" and ")} " +
          s"${if (funds.size == 1) "is" else "are"} guarded by public test signer keys, whose secrets are public: " +
          "anyone can spend these funds. This must never be a public network; set allowTestSignerKeys = false for " +
          "anything other than a private devnet")
      }
    }
  }

  private def tokensError(name: String, box: ErgoBox, id: Array[Byte]): Option[String] = {
    val tokens = box.additionalTokens
    if (tokens.length == 1 && java.util.Arrays.equals(tokens(0)._1.toArray, id) && tokens(0)._2 == 1L) {
      None
    } else {
      Some(s"$name must hold exactly one token, (${Base16.encode(id)}, 1), at tokens(0)")
    }
  }

  private def registersError(name: String,
                             box: ErgoBox,
                             expected: Seq[(NonMandatoryRegisterId, EvaluatedValue[_ <: SType])]): Option[String] = {
    val actual = box.additionalRegisters
    val wrongSet = actual.keySet != expected.map(_._1).toSet
    val wrong = expected.collect {
      case (reg, value) if !actual.get(reg).contains(value) || actual(reg).tpe != value.tpe =>
        s"R${reg.number} must be ${value.tpe} $value"
    }
    if (!wrongSet && wrong.isEmpty) {
      None
    } else {
      val regs = expected.map(r => s"R${r._1.number}").mkString(", ")
      Some(s"$name must have registers $regs only, as listed: " +
        (if (wrongSet) s"has ${actual.keys.map(r => s"R${r.number}").toSeq.sorted.mkString(", ")}; " else "") +
        wrong.mkString("; "))
    }
  }

  /**
    * Condition 5, on the built box: the emission box is at the emission tree, holds exactly `tokens(0)` =
    * (emission NFT id, 1), and has only R4 and R5, `Coll[Byte]`, equal to the hashes of the treasury and LP
    * accumulation trees. Needs condition 1.
    */
  def emissionBoxError(e: YoloEmissionSettings, box: ErgoBox): Option[String] = {
    val errors = Seq(
      if (java.util.Arrays.equals(box.propositionBytes, e.emissionTree.bytes)) None
      else Some("emission box is not at the emission tree"),
      if (box.value == e.genesisValue) None else Some(s"emission box value ${box.value} is not genesisValue"),
      tokensError("emission box", box, e.emissionNftIdBytes),
      registersError("emission box", box, Seq(
        R4 -> ByteArrayConstant(treeHash(e.treasuryAccumulationTree)),
        R5 -> ByteArrayConstant(treeHash(e.lpAccumulationTree))))
    ).flatten
    if (errors.isEmpty) None else fail(s"the genesis emission box is wrong: ${errors.mkString("; ")}")
  }

  /**
    * Condition 6, on the built boxes: fund box trees, tokens and register layouts as listed in the object
    * description; whitelist hashes 32 bytes, heights 0 (genesis), at most 64 entries. Needs condition 1.
    */
  def fundBoxesError(e: YoloEmissionSettings, treasuryBox: ErgoBox, lpBox: ErgoBox): Option[String] = {
    val treasuryErrors = Seq(
      if (java.util.Arrays.equals(treasuryBox.propositionBytes, e.treasuryFundTree.bytes)) None
      else Some("treasury fund box is not at the treasury fund tree"),
      tokensError("treasury fund box", treasuryBox, e.treasuryNftIdBytes),
      registersError("treasury fund box", treasuryBox, Seq(
        R4 -> ByteArrayConstant(ZeroHash),
        R5 -> LongConstant(0L),
        R6 -> ByteArrayConstant(Array.emptyByteArray),
        R7 -> LongConstant(0L),
        R8 -> LongConstant(0L)))
    ).flatten
    val whitelist: Option[Seq[(Coll[Byte], Long)]] = lpBox.additionalRegisters.get(R4) match {
      case Some(c: Constant[_]) if c.tpe == WhitelistType =>
        Some(c.value.asInstanceOf[Coll[(Coll[Byte], Long)]].toArray.toSeq)
      case _ => None
    }
    val whitelistErrors = whitelist match {
      case None =>
        Seq(s"LP fund box R4 must be $WhitelistType")
      case Some(entries) =>
        val count =
          if (entries.size <= MaxWhitelistEntries) None
          else Some(s"lpWhitelist has ${entries.size} entries, at most $MaxWhitelistEntries allowed")
        val bad = entries.zipWithIndex.flatMap { case ((hash, height), i) =>
          Seq(
            if (hash.length == YoloEmissionSettings.IdLength) None
            else Some(s"lpWhitelist($i).hash is ${hash.length} bytes, expected ${YoloEmissionSettings.IdLength}"),
            if (height == GenesisWhitelistHeight) None
            else Some(s"lpWhitelist($i).height is $height, a genesis entry must have height $GenesisWhitelistHeight")
          ).flatten
        }
        val expected = e.lpWhitelist.map(w => (Base16.decode(w.hash).getOrElse(Array.emptyByteArray).toSeq, w.height))
        val configured = entries.map { case (hash, height) => (hash.toArray.toSeq, height) } == expected
        (count.toSeq ++ bad) ++ (if (configured) Nil else Seq("LP fund box R4 is not the configured lpWhitelist"))
    }
    val lpErrors = Seq(
      if (java.util.Arrays.equals(lpBox.propositionBytes, e.lpFundTree.bytes)) None
      else Some("LP fund box is not at the LP fund tree"),
      tokensError("LP fund box", lpBox, e.lpNftIdBytes),
      if (lpBox.additionalRegisters.keySet == Set[NonMandatoryRegisterId](R4, R5, R6, R7)) None
      else Some("LP fund box must have registers R4, R5, R6, R7 only"),
      if (lpBox.additionalRegisters.get(R5).contains(LongConstant(0L))) None else Some("LP fund box R5 must be Long 0"),
      if (lpBox.additionalRegisters.get(R6).contains(LongConstant(0L))) None else Some("LP fund box R6 must be Long 0"),
      if (lpBox.additionalRegisters.get(R7).contains(ByteArrayConstant(ZeroHash))) None
      else Some("LP fund box R7 must be Coll[Byte] of 32 zero bytes")
    ).flatten ++ whitelistErrors
    val all = treasuryErrors ++ lpErrors
    if (all.isEmpty) None else fail(s"the genesis fund boxes are wrong: ${all.mkString("; ")}")
  }

  /** Miner key used only to size the coinbase miner output: every P2PK key serializes to the same length. */
  private val SizingMinerPk: ProveDlog = ProveDlog(CryptoConstants.dlogGroup.generator)

  /**
    * Whether the genesis value strands the emission box: with `v` the value of the last emission box (the one spent
    * at the terminal height, the smallest emission box ever created), the box itself, and the treasury, LP and
    * miner outputs of the terminal spend, must each hold at least `Parameters.MinValueMax` (the largest voteable
    * `minValuePerByte`) per byte, sized on the outputs `YoloEmissionTx.build` makes. Otherwise a vote of
    * `minValuePerByte` can make the last spends dust and the emission box unspendable. Needs condition 1 and a
    * positive genesis value.
    */
  def strandedEmissionError(e: YoloEmissionSettings): Option[String] = {
    val perByte = Parameters.MinValueMax.toLong
    Try {
      val rules = new YoloEmissionRules(e.genesisValue)
      val t = rules.terminalHeight
      val v = rules.boxValueAt(t)
      val h = math.min(t, Int.MaxValue.toLong).toInt
      val nft = emissionBox(e)
      def boxAt(value: Long, height: Int): ErgoBox =
        new ErgoBoxCandidate(value, e.emissionTree, math.max(height, 0), nft.additionalTokens, nft.additionalRegisters)
          .toBox(bytesToId(Array.fill(32)(0: Byte)), 0)
      val terminal = YoloEmissionTx.build(boxAt(v, h - 1), h, SizingMinerPk, e).outputs
      val successor = YoloEmissionTx.build(
        boxAt(Math.addExact(v, YoloEmissionRules.rewardAtHeight(h - 1)), h - 2), h - 1, SizingMinerPk, e).outputs.head
      require(successor.value == v && terminal.size == 3, "unexpected coinbase layout")
      Seq(
        ("the last emission box", v, successor.bytes.length),
        ("the terminal treasury share", terminal(0).value, terminal(0).bytes.length),
        ("the terminal LP share", terminal(1).value, terminal(1).bytes.length),
        ("the terminal miner output", terminal(2).value, terminal(2).bytes.length)
      ).collect { case (name, value, size) if value < perByte * size =>
        s"$name holds $value, below ${perByte * size} (MinValueMax $perByte x $size bytes)"
      } match {
        case Seq() => None
        case failing => Some(s"genesisValue = ${e.genesisValue} strands the emission box: the last emission box " +
          s"(spent at height $t) holds v = $v; ${failing.mkString("; ")}")
      }
    } match {
      case Success(err) => err
      case Failure(err) =>
        Some(s"genesisValue = ${e.genesisValue}: the last emission spend cannot be built (${err.getMessage})")
    }
  }

  /**
    * Condition 7: each fund box holds at least `FundFloor` (100 YOLO), the contracts' storage rent floor, and the
    * genesis emission value is positive and does not strand the emission box (`strandedEmissionError`).
    */
  def valuesError(e: YoloEmissionSettings): Option[String] = {
    val errors = Seq(
      if (e.genesisValue > 0) strandedEmissionError(e) else Some(s"genesisValue = ${e.genesisValue}, must be positive"),
      if (e.treasuryFundValue >= FundFloor) None
      else Some(s"treasuryFundValue = ${e.treasuryFundValue}, must be at least $FundFloor (100 YOLO, the fund floor)"),
      if (e.lpFundValue >= FundFloor) None
      else Some(s"lpFundValue = ${e.lpFundValue}, must be at least $FundFloor (100 YOLO, the fund floor)")
    ).flatten
    if (errors.isEmpty) None else fail(s"$ConfigPath values are wrong: ${errors.mkString("; ")}")
  }

  /** AVL+ digest of `boxes`, computed as `UtxoState.fromBoxHolder` computes the genesis state digest. */
  def stateDigest(boxes: Seq[ErgoBox]): ADDigest = {
    val p = new BatchAVLProver[Digest32, HF](keyLength = 32, valueLengthOpt = None)
    BoxHolder(boxes).sortedBoxes.foreach(b => p.performOneOperation(Insert(b.id, ADValue @@ b.bytes)).get)
    p.digest
  }

  /**
    * Condition 8: `ergo.chain.genesisStateDigestHex` is the digest of the YOLO genesis `boxes` (`stateDigest`). The
    * message gives the computed digest, to set `genesisStateDigestHex` from.
    */
  def digestError(chainSettings: ChainSettings, boxes: Seq[ErgoBox]): Option[String] = {
    val computed = Base16.encode(stateDigest(boxes))
    if (computed == Base16.encode(chainSettings.genesisStateDigest)) {
      None
    } else {
      fail(s"ergo.chain.genesisStateDigestHex = ${chainSettings.genesisStateDigestHex} is not the digest of the YOLO " +
        "genesis boxes; computed: genesisStateDigestHex = \"" + computed + "\"")
    }
  }

  /**
    * The YOLO launch check: the first failing condition of 1 to 8 (4b and 4c after 4), in order, or None if the YOLO
    * genesis inputs are consistent. None with the switch off (nothing is checked).
    */
  def launchError(chainSettings: ChainSettings): Option[String] = {
    if (!chainSettings.yolo.enabled) {
      None
    } else {
      val e = chainSettings.yolo.emission
      inputsError(e)
        .orElse(idsError(e))
        .orElse(Try(genesisBoxes(chainSettings)) match {
          case Failure(t) => fail(s"the YOLO genesis boxes cannot be built: ${t.getMessage}")
          case Success(boxes) =>
            boxIdCollisionError(e, boxes)
              .orElse(treeIdsError(e))
              .orElse(treePinsError(e))
              .orElse(testSignerKeysError(e))
              .orElse(emissionBoxError(e, boxes.head))
              .orElse(fundBoxesError(e, boxes(1), boxes(2)))
              .orElse(valuesError(e))
              .orElse(digestError(chainSettings, boxes))
        })
    }
  }

}
