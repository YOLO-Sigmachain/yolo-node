package org.ergoplatform.settings

import org.ergoplatform.ErgoBox
import scorex.util.encode.Base16
import sigma.VersionContext
import sigma.ast.ErgoTree
import sigma.serialization.ErgoTreeSerializer

import scala.util.Try

/**
  * YOLO (SigmaChain) chain switch and YOLO-only chain settings, read from `ergo.chain.yolo`.
  *
  * The YOLO launch parameters, rule activation heights, soft-fork voting start height and difficulty rule
  * (EIP-37 from genesis with a first-retarget guard and a floor, see `HeadersProcessor.requiredDifficultyAfter`)
  * are gated by `enabled`, which defaults to `false`. The PoW height divisor is NOT gated by this switch: it is
  * a separate setting, `ergo.chain.powScheme.powHeightDivisor` (default 1, YOLO 4). A node with one
  * set and not the other would reject or orphan every block, so `ErgoSettingsReader` refuses to start
  * unless `enabled` is true exactly when the divisor is not 1, and only on networkType `testnet` with
  * a non-mainnet address prefix; it also refuses unknown keys in this section (e.g. `enable`).
  * With the switch off and divisor 1 the node behaves as upstream Ergo. The whole section may be
  * omitted from a config: every field has a default.
  *
  * @param enabled                   - the YOLO switch. When `false`, no other field here is read
  *                                    by consensus or mining code
  * @param softForkVotingStartHeight - first height at which the candidate generator may vote for a
  *                                    soft-fork (in place of the hard-coded Ergo heights, see
  *                                    `CandidateGenerator.forkOrdered`). Used only when `enabled`
  * @param emission                  - inputs for the YOLO emission and fund contracts (genesis boxes,
  *                                    emission box tracking, and later the coinbase builder)
  */
case class YoloSettings(enabled: Boolean = false,
                        softForkVotingStartHeight: Int = 0,
                        emission: YoloEmissionSettings = YoloEmissionSettings())

object YoloSettings {

  /**
    * The switch-off value, used when `ergo.chain.yolo` is absent from the config.
    */
  val disabled: YoloSettings = YoloSettings()

  /**
    * Activation height of validation rule 110 (`txDataInputsUnique`) on YOLO: active from genesis.
    */
  val DataInputsUniquenessHeight: Int = 0

  /**
    * Activation height of validation rule 125 (`txRentDistinctOutputs`) on YOLO: active from genesis.
    */
  val StorageRentDistinctOutputsActivationHeight: Int = 0

  /**
    * Genesis storage fee factor on YOLO, in nanocoins per byte per storage period
    * (Ergo: `Parameters.StorageFeeFactorDefault` = 1,250,000). Used by `YoloLaunchParameters`.
    * Must be a multiple of `Parameters.StorageFeeFactorStep` (25,000): voting moves the value one step
    * at a time and only checks the bounds before stepping, so an off-grid value can leave [min, max].
    */
  val StorageFeeFactor: Int = 300000

  /**
    * Genesis maximum block cost on YOLO (Ergo: `Parameters.MaxBlockCostDefault` = 1,000,000). Used by
    * `YoloLaunchParameters`. Voteable afterwards like any other parameter, in steps of 1% of the current value.
    * The per-transaction relay limit is a node setting, `ergo.node.maxTransactionCost` (1,000,000 in yolo-testnet.conf).
    */
  val MaxBlockCost: Int = 1500000

  /**
    * Difficulty floor on YOLO: an EIP-37 retarget never returns less (`DifficultyAdjustment.yoloEip37Calculate`),
    * and the start-up check refuses an initial difficulty below it. At difficulty 1 EIP-37 cannot climb
    * (1 * 3 / 2 = 1) and after a slow epoch falls to 0, which makes the PoW target q / 0 and halts the chain.
    * Deliberately tiny: it only removes those dead states. A constant, not a config value, so that no node can
    * restart with a different floor and fork itself off. Exactly representable in nBits.
    */
  val MinDifficulty: BigInt = BigInt(2)

  /**
    * EIP-37 epoch length on YOLO: 64 blocks, 32 minutes at 30 s blocks (Ergo mainnet: 128). With the first-retarget
    * guard the first retarget is at block 129. `ergo.chain.eip37EpochLength` must equal it when the switch
    * is on (`ErgoSettingsReader.yoloSettingsError`): the retarget heights follow from it, so a node with another
    * value would retarget at heights no other node does and fork itself off. A constant for the same reason as
    * `MinDifficulty`. Not the voting epoch, which is `ergo.chain.voting.votingLength` and is not tied to this value.
    */
  val Eip37EpochLength: Int = 64

  /**
    * Future-timestamp limit for headers on YOLO, in block intervals: 4, so 2 minutes at 30 s blocks (Ergo: 10).
    * Used by `ChainSettings.maxTimeDrift`. A smaller limit narrows how far one miner can move the next difficulty
    * with a false timestamp on the last block of an epoch. A constant, as every node must apply the same limit.
    */
  val MaxTimeDriftBlocks: Int = 4

}

/**
  * One entry of the LP fund genesis whitelist (lp_fund.es register R4, `Coll[(Coll[Byte], Long)]`).
  *
  * @param hash   - hex of the 32-byte blake2b256 hash of an allowed destination's ErgoTree bytes
  * @param height - height the entry is stamped with; the start-up check allows only 0 (genesis)
  */
case class YoloWhitelistEntry(hash: String = "", height: Long = 0L)

/**
  * Inputs for the YOLO genesis boxes, emission box tracking and coinbase.
  * Defaults are empty; with the YOLO switch on, `ErgoSettingsReader` refuses to start unless every field is set
  * and the launch check in `YoloGenesis.launchError` passes. With the switch off nothing here is read.
  *
  * @param emissionTreeHex             - hex of the serialized ErgoTree of the emission contract (emission.es)
  * @param treasuryAccumulationTreeHex - hex of the ErgoTree of the treasury accumulation contract
  *                                      (treasury_accumulation.es), the script treasury shares are paid to
  * @param lpAccumulationTreeHex       - hex of the ErgoTree of the LP accumulation contract (lp_accumulation.es),
  *                                      the script LP shares are paid to
  * @param treasuryFundTreeHex         - hex of the ErgoTree of the treasury fund contract (treasury_governance.es)
  * @param lpFundTreeHex               - hex of the ErgoTree of the LP fund contract (lp_fund.es)
  * @param emissionNftId               - hex of the 32-byte emission NFT id carried by the emission box
  * @param treasuryNftId               - hex of the 32-byte treasury NFT id carried by the treasury fund box
  * @param lpNftId                     - hex of the 32-byte LP NFT id carried by the LP fund box
  * @param genesisValue                - value of the genesis emission box, in nanocoins
  * @param treasuryFundValue           - value of the genesis treasury fund box, in nanocoins
  * @param lpFundValue                 - value of the genesis LP fund box, in nanocoins
  * @param lpWhitelist                 - LP fund genesis whitelist (register R4 of the LP fund box)
  * @param allowTestSignerKeys         - whether the fund trees may hold the three public test signer keys of the
  *                                      contract sources (`YoloTreePins.TestSignerKeys`). False by default; only
  *                                      yolo-devnet.conf, for a local devnet, sets it. With false the launch check
  *                                      refuses a fund tree that holds any of them
  */
case class YoloEmissionSettings(emissionTreeHex: String = "",
                                treasuryAccumulationTreeHex: String = "",
                                lpAccumulationTreeHex: String = "",
                                treasuryFundTreeHex: String = "",
                                lpFundTreeHex: String = "",
                                emissionNftId: String = "",
                                treasuryNftId: String = "",
                                lpNftId: String = "",
                                genesisValue: Long = 0L,
                                treasuryFundValue: Long = 0L,
                                lpFundValue: Long = 0L,
                                lpWhitelist: Seq[YoloWhitelistEntry] = Seq.empty,
                                allowTestSignerKeys: Boolean = false) {
  import YoloEmissionSettings._

  /** Emission contract tree. Throws if `emissionTreeHex` is malformed (the start-up check refuses that). */
  lazy val emissionTree: ErgoTree = parseTree(emissionTreeHex).get

  /** Treasury accumulation contract tree. Throws if malformed. */
  lazy val treasuryAccumulationTree: ErgoTree = parseTree(treasuryAccumulationTreeHex).get

  /** LP accumulation contract tree. Throws if malformed. */
  lazy val lpAccumulationTree: ErgoTree = parseTree(lpAccumulationTreeHex).get

  /** Treasury fund contract tree. Throws if malformed. */
  lazy val treasuryFundTree: ErgoTree = parseTree(treasuryFundTreeHex).get

  /** LP fund contract tree. Throws if malformed. */
  lazy val lpFundTree: ErgoTree = parseTree(lpFundTreeHex).get

  /** Emission NFT id bytes. Throws if `emissionNftId` is not 32 bytes of hex. */
  lazy val emissionNftIdBytes: Array[Byte] = decodeId(emissionNftId).get

  /** Treasury NFT id bytes. Throws if malformed. */
  lazy val treasuryNftIdBytes: Array[Byte] = decodeId(treasuryNftId).get

  /** LP NFT id bytes. Throws if malformed. */
  lazy val lpNftIdBytes: Array[Byte] = decodeId(lpNftId).get

  // bytes for the per-block emission box test; empty (so matching no box) if unset or malformed
  private lazy val emissionTreeBytesOrEmpty: Array[Byte] =
    Base16.decode(emissionTreeHex).getOrElse(Array.emptyByteArray)
  private lazy val emissionNftIdOrEmpty: Array[Byte] = decodeId(emissionNftId).getOrElse(Array.emptyByteArray)

  /**
    * Whether `box` is the YOLO emission box: its tree bytes equal the emission tree AND `tokens(0)` is
    * (emission NFT id, 1). The tree alone does not identify the box, as anyone can create a lookalike box at the
    * emission script. False for every box if the inputs are unset or malformed.
    */
  def isEmissionBox(box: ErgoBox): Boolean = {
    val tokens = box.additionalTokens
    emissionTreeBytesOrEmpty.nonEmpty && emissionNftIdOrEmpty.nonEmpty &&
      java.util.Arrays.equals(box.propositionBytes, emissionTreeBytesOrEmpty) &&
      tokens.nonEmpty &&
      java.util.Arrays.equals(tokens(0)._1.toArray, emissionNftIdOrEmpty) &&
      tokens(0)._2 == 1L
  }

}

object YoloEmissionSettings {

  /** Length of a token (NFT) id and of a whitelist hash, in bytes. */
  val IdLength: Int = 32

  /**
    * Script version the trees are parsed under: YOLO genesis runs block version 4 (`YoloLaunchParameters`), so the
    * activated script version is 3 (Sigma 6.0).
    */
  val ScriptVersion: Byte = VersionContext.V6SoftForkVersion

  /**
    * Parses hex as an ErgoTree, strictly: the hex must decode, the tree must parse to a proposition (not an
    * unparsed soft-fork tree) and serialize back to exactly the given bytes (so no trailing bytes).
    */
  def parseTree(hex: String): Try[ErgoTree] = Try {
    require(hex.nonEmpty, "empty")
    val bytes = Base16.decode(hex).getOrElse(throw new IllegalArgumentException("not hex"))
    val tree = VersionContext.withVersions(ScriptVersion, ScriptVersion) {
      ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(bytes)
    }
    tree.root match {
      case Left(unparsed) => throw new IllegalArgumentException(s"does not parse: ${unparsed.error.getMessage}")
      case Right(_) =>
    }
    require(java.util.Arrays.equals(tree.bytes, bytes), "does not serialize back to the same bytes")
    tree
  }

  /** Decodes hex as a 32-byte id. */
  def decodeId(hex: String): Try[Array[Byte]] = Try {
    require(hex.nonEmpty, "empty")
    val bytes = Base16.decode(hex).getOrElse(throw new IllegalArgumentException("not hex"))
    require(bytes.length == IdLength, s"${bytes.length} bytes, expected $IdLength")
    bytes
  }

}
