package org.ergoplatform.settings

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
  */
case class YoloSettings(enabled: Boolean = false,
                        softForkVotingStartHeight: Int = 0)

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
