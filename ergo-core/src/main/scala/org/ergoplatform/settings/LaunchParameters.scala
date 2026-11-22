package org.ergoplatform.settings

import org.ergoplatform.modifiers.history.header.Header

/**
  * Parameters corresponding to initial moment of time in the mainnet and the testnet
  */
object MainnetLaunchParameters extends Parameters(height = 0,
  parametersTable = Parameters.DefaultParameters,
  proposedUpdate = ErgoValidationSettingsUpdate.empty)

/**
  * Parameters corresponding to the genesis block in the public testnet
  */
object TestnetLaunchParameters extends Parameters(height = 0,
  parametersTable = Parameters.DefaultParameters.updated(Parameters.BlockVersion, Header.Interpreter60Version),
  proposedUpdate = ErgoValidationSettingsUpdate(Seq(215, 409), Seq.empty))

/**
  * Initial parameters corresponding to a devnet which is starting with 5.0 activated
  */
object DevnetLaunchParameters extends Parameters(height = 0,
  parametersTable = Parameters.DefaultParameters.updated(Parameters.BlockVersion, Header.Interpreter50Version),
  proposedUpdate = ErgoValidationSettingsUpdate.empty)

/**
  * Initial parameters corresponding to a devnet which is starting with 6.0 activated
  */
object Devnet60LaunchParameters extends Parameters(height = 0,
  parametersTable = Parameters.DefaultParameters.updated(Parameters.BlockVersion, Header.Interpreter60Version),
  proposedUpdate = ErgoValidationSettingsUpdate.empty)

/**
  * Parameters corresponding to the genesis block of a YOLO (SigmaChain) network, selected by
  * `ErgoSettings.launchParameters` when `ergo.chain.yolo.enabled = true`.
  *
  * Same table and rule disables as `TestnetLaunchParameters` (block version 4, rules 215 and 409
  * disabled), except that the storage fee factor starts at `YoloSettings.StorageFeeFactor` and the maximum block
  * cost at `YoloSettings.MaxBlockCost`.
  */
object YoloLaunchParameters extends Parameters(height = 0,
  parametersTable = TestnetLaunchParameters.parametersTable
    .updated(Parameters.StorageFeeFactorIncrease, YoloSettings.StorageFeeFactor)
    .updated(Parameters.MaxBlockCostIncrease, YoloSettings.MaxBlockCost),
  proposedUpdate = TestnetLaunchParameters.proposedUpdate)
