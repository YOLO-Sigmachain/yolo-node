package org.ergoplatform.nodeView.state

import scorex.crypto.hash.Blake2b256
import scorex.util.encode.Base16
import sigma.VersionContext
import sigma.ast.{Constant, ErgoTree, SType, SigmaPropConstant}
import sigma.data.{CSigmaProp, ProveDlog}
import sigma.serialization.ValueSerializer
import org.ergoplatform.settings.YoloEmissionSettings

/**
  * Contract identity pins of the YOLO genesis trees (launch check 4b, `YoloGenesis.treePinsError`).
  *
  * A configured tree is accepted for a role only if its header byte, the blake2b256 hash of its template
  * (`ErgoTree.template`, the tree with constants as placeholders), its number of segregated constants and the value
  * of every constant equal the pin, except the free constants: the NFT id at `idIndex` (which must be the role's
  * own id) and, for the two fund trees, the three signer keys at `keyIndices` (which must be distinct P2PK keys).
  * So the only free parameters of a tree are its NFT id and, for a fund tree, its three keys.
  *
  * THESE PINS MUST BE REGENERATED from the release-compiled trees whenever a contract source changes (emission.es,
  * treasury_accumulation.es and lp_accumulation.es, treasury_governance.es, lp_fund.es) or the compiler does.
  * `YoloTreePinsSpec` derives them from the trees in yolo-devnet.conf, prints them as source and fails if they
  * differ from the values below.
  */
object YoloTreePins {

  /**
    * Pin of one contract role.
    *
    * @param role         - role name, used in messages
    * @param header       - ErgoTree header byte
    * @param templateHash - hex of blake2b256(`ErgoTree.template`)
    * @param constants    - hex of every segregated constant as the tree serializes it (type, then value), in order;
    *                       the free entries (`idIndex` and `keyIndices`) are empty strings
    * @param idIndex      - index of the NFT id constant
    * @param keyIndices   - indices of the three signer-key constants of a fund tree, empty for the other roles
    */
  case class TreePin(role: String,
                     header: Byte,
                     templateHash: String,
                     constants: IndexedSeq[String],
                     idIndex: Int,
                     keyIndices: Seq[Int]) {
    require(constants.indices.filter(constants(_).isEmpty) == (idIndex +: keyIndices).sorted,
      s"$role pin: the empty constants must be exactly the id and key indices")

    /** Number of segregated constants of the tree. */
    def constantCount: Int = constants.length
  }

  /** Hex of a constant as the tree serializes it: type, then value. */
  def constantHex(c: Constant[SType]): String = VersionContext.withVersions(
    YoloEmissionSettings.ScriptVersion, YoloEmissionSettings.ScriptVersion) {
    Base16.encode(ValueSerializer.serialize(c))
  }

  /** Hex of blake2b256 of a tree's template. */
  def templateHash(tree: ErgoTree): String = Base16.encode(Blake2b256(tree.template))

  /** Whether a constant is a P2PK key (`PK(...)` in the contract source). */
  def isProveDlog(c: Constant[SType]): Boolean = c match {
    case SigmaPropConstant(CSigmaProp(_: ProveDlog)) => true
    case _ => false
  }

  /**
    * The pin of `tree` for `role`, with the constants at `idIndex` and `keyIndices` left free. Used by
    * `YoloTreePinsSpec` to regenerate the pins below.
    */
  def derive(role: String, tree: ErgoTree, idIndex: Int, keyIndices: Seq[Int]): TreePin = {
    val free = (idIndex +: keyIndices).toSet
    TreePin(role, tree.header, templateHash(tree),
      tree.constants.indices.map(i => if (free.contains(i)) "" else constantHex(tree.constants(i))),
      idIndex, keyIndices)
  }

  /**
    * Mismatches of `tree` against `pin`, with `ownId` (named `idName` in messages) expected at the id index; empty
    * if the tree is the pinned contract.
    */
  def mismatches(pin: TreePin, tree: ErgoTree, ownId: Array[Byte], idName: String): Seq[String] = {
    val hash = templateHash(tree)
    if (tree.header != pin.header) {
      Seq(f"header byte ${tree.header}%02x, expected ${pin.header}%02x")
    } else if (hash != pin.templateHash) {
      Seq(s"template hash $hash, expected ${pin.templateHash}")
    } else if (tree.constants.length != pin.constantCount) {
      Seq(s"${tree.constants.length} constants, expected ${pin.constantCount}")
    } else {
      val consts = tree.constants
      val idError = consts(pin.idIndex) match {
        case c if constantHex(c) == "0e20" + Base16.encode(ownId) => None
        case _ => Some(s"constant ${pin.idIndex} is not the $idName NFT id")
      }
      val valueErrors = consts.indices.collect {
        case i if pin.constants(i).nonEmpty && constantHex(consts(i)) != pin.constants(i) =>
          s"constant $i is ${constantHex(consts(i))}, expected ${pin.constants(i)}"
      }
      val keys = pin.keyIndices.map(consts(_))
      val keyTypeErrors = pin.keyIndices.collect { case i if !isProveDlog(consts(i)) => s"constant $i is not a P2PK key" }
      val keyDuplicates =
        if (keyTypeErrors.isEmpty && keys.map(constantHex).distinct.size != keys.size) {
          Seq(s"the signer keys (constants ${pin.keyIndices.mkString(", ")}) are not pairwise distinct")
        } else {
          Seq.empty
        }
      idError.toSeq ++ valueErrors ++ keyTypeErrors ++ keyDuplicates
    }
  }

  /**
    * The three public test signer keys, as hex of the compressed public key, that treasury_governance.es and
    * lp_fund.es are written with and that the fund trees in yolo-devnet.conf embed. The contract sources give them as
    * the P2PK addresses 9gzkoMXatUr5s7jMBvjR7hzJyPqAyvZtdYxHZ9giJUEyvZ9nJde,
    * 9fZM68VSqtjH3HibZngnQ9sgZXudKBt146mkKHWuur3qV4DKDYk and 9eYPis3GAjApr8RQKADhk4ZDaNQC7cM46pJi91jzJDa2RoymEzh.
    * They are public and must never guard real funds, so the launch check refuses a fund tree that holds any of them
    * unless `ergo.chain.yolo.emission.allowTestSignerKeys` is true (`YoloGenesis.testSignerKeysError`).
    */
  val TestSignerKeys: Seq[String] = Seq(
    "034646ae5047316b4230d0086c8acec687f00b1cd9d1dc634f6cb358ac0a9a8fff",
    "0288e2ddeb04657dbd0edadf9c1f98da3b3895faa1f00527934dd35d17542ffe9b",
    "020305c75318f36537e1a5d0db4dfcdc94a9708a84a38f81ea7cfbe239252e01f2")

  // a P2PK constant serializes as 08 (SigmaProp type), cd (ProveDlog), then the 33-byte compressed key
  private val TestSignerKeyConstants: Set[String] = TestSignerKeys.map("08cd" + _).toSet

  /** Whether a constant is one of the three public test signer keys (`TestSignerKeys`). */
  def isTestSignerKey(c: Constant[SType]): Boolean = isProveDlog(c) && TestSignerKeyConstants.contains(constantHex(c))

  /**
    * Indices of the signer-key constants of `tree`, among the pin's `keyIndices`, that hold a public test signer key.
    * Needs a tree that matches `pin` (`mismatches` empty).
    */
  def testSignerKeyIndices(pin: TreePin, tree: ErgoTree): Seq[Int] =
    pin.keyIndices.filter(i => isTestSignerKey(tree.constants(i)))

  /** emission.es v1.4. */
  val Emission: TreePin = TreePin("emission", 0x10, "e5be2df74d21a409b91cb9a3cc8b180234a37ffb0adb6acddb8773f0caf0d32f", IndexedSeq(
    "", "0402", "0480a98001", "0400",
    "0580b8c9e5ae04", "0402", "0580dce4b29702", "0404",
    "0580aeb2d98b01", "0406", "058097d9ec45", "0408",
    "05c0cbacf622", "040a", "05e0a596bb11", "0580bcc1960b",
    "0580bcc1960b", "0580bcc1960b", "0400", "0508",
    "05c801", "0400", "0402", "0506",
    "05c801", "0508", "05c801", "0404",
    "0506", "05c801", "0400", "0502",
    "0e07100204802d08cd", "0e0eea02d192a39a8cc7a70173007301", "0400", "0402",
    "0400", "0400", "0502", "0408",
    "0406"),
    0, Seq())

  /** treasury_accumulation.es and lp_accumulation.es (one contract, shared by the treasury and LP roles). */
  val Accumulation: TreePin = TreePin("accumulation", 0x10, "6635d9a61c445dc3acdad0bbe99c7831572de84c6c45956d80015333ce55fa54", IndexedSeq(
    "0400", "0400", "0400", "",
    "0502", "0400", "0500", "0400",
    "0400", "0480bb02", "0414", "0500"),
    3, Seq())

  /** treasury_governance.es (the treasury fund); signer keys at 11, 12, 13. */
  val TreasuryFund: TreePin = TreePin("treasury fund", 0x10, "23a0b6a57e235823f9115f70ae8c657a3413ce6486600b9f2382331466a91022", IndexedSeq(
    "", "0400", "0500", "05808701",
    "0580bb02", "0400", "0400", "0502",
    "0504", "0580897a", "0404", "",
    "", "", "0400", "0400",
    "0502", "0500", "0404", "0514",
    "0500", "0502", "0580a0b787e905", "0580a0b787e905",
    "04a00b", "0500", "0500", "0402",
    "0404",
    "0402",
    "0e691005040004000e36100204a00b08cd0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798" +
      "ea02d192a39a8cc7a701730073011001020402d19683030193a38cc7b2a57300000193c2b2a57301007473027303830108cd" +
      "eeac93b1a57304",
    "0580dac409",
    "0500", "0500", "0500", "0500",
    "0500", "0502", "0502", "0500",
    "0500", "0504", "0440", "0400",
    "0500", "0500", "0500", "0500",
    "0500", "0500", "0500"),
    0, Seq(11, 12, 13))

  /** lp_fund.es (the LP fund); signer keys at 12, 13, 14. */
  val LpFund: TreePin = TreePin("LP fund", 0x10, "abc56ab694debc54367e97a3cfc60fa49d546f47d4efb3b776f5824c32ebe500", IndexedSeq(
    "", "0400", "0400", "0400",
    "0502", "0504", "0500", "05808701",
    "05808701", "0580bb02", "0580897a", "0404",
    "", "", "", "0400",
    "0400", "0502", "0502", "0402",
    "0e691005040004000e36100204a00b08cd0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798" +
      "ea02d192a39a8cc7a701730073011001020402d19683030193a38cc7b2a57300000193c2b2a57301007473027303830108cd" +
      "eeac93b1a57304",
    "0580dac409",
    "0580a0b787e905",
    "0580a0b787e905",
    "04a00b", "0402", "05808701", "048001",
    "0400", "0440", "0514", "0500",
    "0502", "0502", "0500", "0500",
    "0504", "0514", "0500", "0440",
    "0500", "0500", "0500", "0500",
    "0500", "0500", "0500", "0500"),
    0, Seq(12, 13, 14))

}
