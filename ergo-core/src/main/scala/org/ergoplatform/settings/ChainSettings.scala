package org.ergoplatform.settings

import sigmastate.utils.Helpers._
import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.mining.AutolykosPowScheme
import org.ergoplatform.mining.difficulty.DifficultySerializer
import org.ergoplatform.mining.emission.EmissionRules
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import scorex.crypto.authds.ADDigest
import scorex.util.ModifierId
import scorex.util.encode.Base16

import scala.concurrent.duration.FiniteDuration

/**
  * Configuration file for Ergo chain
  *
  * @see src/main/resources/application.conf for parameters description
  */
case class ChainSettings(protocolVersion: Byte,
                         addressPrefix: Byte,
                         blockInterval: FiniteDuration,
                         epochLength: Int,
                         eip37EpochLength: Option[Int],
                         useLastEpochs: Int,
                         voting: VotingSettings,
                         powScheme: AutolykosPowScheme,
                         monetary: MonetarySettings,
                         reemission: ReemissionSettings,
                         noPremineProof: Seq[String],
                         foundersPubkeys: Seq[String],
                         genesisStateDigestHex: String,
                         initialDifficultyHex: String,
                         makeSnapshotEvery: Int,
                         genesisId: Option[ModifierId] = None,
                         yolo: YoloSettings = YoloSettings.disabled) {

  val isMainnet: Boolean = addressPrefix == ErgoAddressEncoder.MainnetNetworkPrefix

  val genesisStateDigest: ADDigest = Base16.decode(genesisStateDigestHex)
    .fold(_ => throw new Error(s"Failed to parse genesisStateDigestHex = $genesisStateDigestHex"), ADDigest @@ _)

  val emissionRules: EmissionRules = new EmissionRules(monetary)

  val addressEncoder = new ErgoAddressEncoder(addressPrefix)

  val initialDifficulty: BigInt = Base16.decode(initialDifficultyHex)
    .fold(_ => throw new Error(s"Failed to parse initialDifficultyHex = $initialDifficultyHex"), BigInt(_))

  /**
    * Initial (genesis block) difficulty encoded as nbits
    */
  val initialNBits: Long = DifficultySerializer.encodeCompactBits(initialDifficulty)

  val initialDifficultyVersion2: BigInt = Base16.decode(voting.version2ActivationDifficultyHex)
    .fold(_ => throw new Error(s"Failed to parse initialDifficultyVersion2 = $initialDifficultyHex"), BigInt(_))

  /**
    * Height since which validation rule 110 (`txDataInputsUnique`) is applied.
    * Upstream value unless the YOLO switch is on, in which case the rule is active from genesis.
    */
  def dataInputsUniquenessHeight: Int =
    if (yolo.enabled) YoloSettings.DataInputsUniquenessHeight else ErgoTransaction.DataInputsUniquenessHeight

  /**
    * Height since which validation rule 125 (`txRentDistinctOutputs`) is applied.
    * Upstream value unless the YOLO switch is on, in which case the rule is active from genesis.
    */
  def storageRentDistinctOutputsActivationHeight: Int =
    if (yolo.enabled) {
      YoloSettings.StorageRentDistinctOutputsActivationHeight
    } else {
      ValidationRules.StorageRentDistinctOutputsActivationHeight
    }

  /**
    * Whether validation rule 308 (`bsStorageRentAttestation`) applies to a block of version `blockVersion`.
    * Upstream: from `Header.StorageRentAttestationVersion` on. Used both by the validator (`ErgoState.execTransactions`) and by the
    * candidate generator (`CandidateGenerator.storageRentAttestation`), so producer and validator agree.
    */
  def storageRentAttestationRequired(blockVersion: Byte): Boolean =
    blockVersion >= Header.StorageRentAttestationVersion

  /**
    * Header version of the genesis block. Upstream: `Header.InitialVersion` (1), so the genesis header is solved
    * with Autolykos v1, which only the node's internal miner supports. With the YOLO switch on, the genesis block
    * has the block version of the launch parameters, as every block after it, so the whole chain is Autolykos v2
    * and the genesis work message for external miners carries the PoW height.
    *
    * @param launchBlockVersion - block version of the launch parameters
    */
  def genesisBlockVersion(launchBlockVersion: Byte): Byte =
    if (yolo.enabled) launchBlockVersion else Header.InitialVersion

  /**
    * How far ahead of the node's clock a header timestamp may be, in milliseconds (validation rule 211,
    * `hdrFutureTimestamp`, checked in `HeadersProcessor`). Upstream: 10 block intervals. With the YOLO switch on:
    * `YoloSettings.MaxTimeDriftBlocks` (4) block intervals, 2 minutes at 30 s blocks. A header beyond the limit is
    * refused as a recoverable error, so it can be accepted later, once the node's clock has caught up.
    */
  def maxTimeDrift: Long =
    (if (yolo.enabled) YoloSettings.MaxTimeDriftBlocks else 10) * blockInterval.toMillis

}
