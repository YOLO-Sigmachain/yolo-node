package org.ergoplatform.mining

import org.ergoplatform.settings.{ErgoSettings, ErgoValidationSettingsUpdate, NetworkType, Parameters, TestnetLaunchParameters, VotingTargets, YoloSettings}
import org.ergoplatform.utils.ErgoCorePropertyTest

/**
  * Soft-fork voting start height: a chain setting when the YOLO switch is on,
  * the upstream hard-coded heights when it is off.
  */
class YoloForkVotingSpec extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.generators.ErgoCoreGenerators._

  // a node at protocol version 4 on a version-3 chain, ordered to vote for the soft-fork (120 = 1),
  // with no soft-fork voting in progress
  private val params = TestnetLaunchParameters
  private val voteForFork = VotingTargets(Map(Parameters.SoftFork -> 1), ErgoValidationSettingsUpdate.empty)

  private def nodeSettings(networkType: NetworkType, yolo: YoloSettings): ErgoSettings =
    settings.copy(
      networkType = networkType,
      chainSettings = settings.chainSettings.copy(protocolVersion = 4, yolo = yolo),
      votingTargets = voteForFork)

  /** Whether the node votes for the soft-fork in the block after one at `height`. */
  private def votes(s: ErgoSettings, height: Int): Boolean = {
    val header = defaultHeaderGen.sample.get.copy(version = 3, height = height)
    CandidateGenerator.forkOrdered(s, params, header)
  }

  property("switch off: upstream hard-coded voting start heights") {
    val testnet = nodeSettings(NetworkType.TestNet, YoloSettings.disabled)
    votes(testnet, 1548798) shouldBe false
    votes(testnet, 1548799) shouldBe true // next height 1,548,800

    val mainnet = nodeSettings(NetworkType.MainNet, YoloSettings.disabled)
    votes(mainnet, 1561599) shouldBe false
    votes(mainnet, 1561600) shouldBe true // next height 1,561,601

    val devnet = nodeSettings(NetworkType.DevNet, YoloSettings.disabled)
    votes(devnet, 6) shouldBe false
    votes(devnet, 7) shouldBe true // next height 8
  }

  property("switch on: voting start height is the chain setting") {
    val fromGenesis = nodeSettings(NetworkType.TestNet, YoloSettings(enabled = true))
    votes(fromGenesis, 1) shouldBe true
    votes(fromGenesis, 1000) shouldBe true

    val from1024 = nodeSettings(NetworkType.TestNet, YoloSettings(enabled = true, softForkVotingStartHeight = 1024))
    votes(from1024, 1022) shouldBe false
    votes(from1024, 1023) shouldBe true // next height 1,024
    // the hard-coded testnet height is no longer the gate
    votes(from1024, 1548799) shouldBe true
  }

  property("switch on: no vote unless ordered, as upstream") {
    val notOrdered = nodeSettings(NetworkType.TestNet, YoloSettings(enabled = true)).copy(votingTargets = VotingTargets.empty)
    votes(notOrdered, 1000) shouldBe false
  }

}
