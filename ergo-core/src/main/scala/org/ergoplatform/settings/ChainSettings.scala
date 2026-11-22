package org.ergoplatform.settings

import sigmastate.utils.Helpers._
import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.mining.AutolykosPowScheme
import org.ergoplatform.mining.difficulty.DifficultySerializer
import org.ergoplatform.mining.emission.EmissionRules
import org.ergoplatform.modifiers.history.header.Header
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
                         genesisId: Option[ModifierId] = None) {

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
    * Whether validation rule 308 (`bsStorageRentAttestation`) applies to a block of version `blockVersion`.
    * Upstream: from `Header.StorageRentAttestationVersion` on. Used both by the validator (`ErgoState.execTransactions`) and by the
    * candidate generator (`CandidateGenerator.storageRentAttestation`), so producer and validator agree.
    */
  def storageRentAttestationRequired(blockVersion: Byte): Boolean =
    blockVersion >= Header.StorageRentAttestationVersion

}
