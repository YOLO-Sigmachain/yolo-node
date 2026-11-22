package org.ergoplatform.settings

import com.typesafe.config.ConfigFactory
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.utils.ErgoCorePropertyTest

/**
  * YOLO switch, config block and the constants it gates in ergo-core (launch parameters, rules 110 and 125).
  */
class YoloSettingsSpec extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoCoreTestConstants._

  private val yoloOn: ChainSettings = chainSettings.copy(yolo = YoloSettings(enabled = true))

  property("the switch is off in the upstream test settings") {
    chainSettings.yolo shouldBe YoloSettings.disabled
    chainSettings.yolo.enabled shouldBe false
    chainSettings.powScheme.powHeightDivisor shouldBe 1
  }

  property("with the switch off, rule 110 and 125 activation heights are the upstream constants") {
    chainSettings.dataInputsUniquenessHeight shouldBe ErgoTransaction.DataInputsUniquenessHeight
    chainSettings.dataInputsUniquenessHeight shouldBe 1885000
    chainSettings.storageRentDistinctOutputsActivationHeight shouldBe
      ValidationRules.StorageRentDistinctOutputsActivationHeight
    chainSettings.storageRentDistinctOutputsActivationHeight shouldBe 1885000
  }

  property("with the switch on, rules 110 and 125 are active from genesis") {
    yoloOn.dataInputsUniquenessHeight shouldBe 0
    yoloOn.storageRentDistinctOutputsActivationHeight shouldBe 0
    // the upstream constants themselves are untouched
    ErgoTransaction.DataInputsUniquenessHeight shouldBe 1885000
    ValidationRules.StorageRentDistinctOutputsActivationHeight shouldBe 1885000
  }

  property("YOLO launch parameters are testnet launch parameters with storage fee factor 300,000 and " +
    "block cost 1,500,000") {
    YoloLaunchParameters.height shouldBe 0
    YoloLaunchParameters.storageFeeFactor shouldBe 300000
    YoloLaunchParameters.maxBlockCost shouldBe 1500000
    YoloLaunchParameters.blockVersion shouldBe Header.Interpreter60Version
    YoloLaunchParameters.parametersTable shouldBe
      TestnetLaunchParameters.parametersTable
        .updated(Parameters.StorageFeeFactorIncrease, 300000)
        .updated(Parameters.MaxBlockCostIncrease, 1500000)
    YoloLaunchParameters.proposedUpdate shouldBe TestnetLaunchParameters.proposedUpdate
    YoloLaunchParameters.proposedUpdate.rulesToDisable shouldBe Seq[Short](215, 409)

    // pinned literally, so that a change to the upstream testnet table cannot silently change YOLO genesis
    YoloLaunchParameters.parametersTable shouldBe Map[Byte, Int](
      (1: Byte) -> 300000,       // StorageFeeFactorIncrease
      (2: Byte) -> 360,          // MinValuePerByteIncrease
      (3: Byte) -> 524288,       // MaxBlockSizeIncrease
      (4: Byte) -> 1500000,      // MaxBlockCostIncrease
      (5: Byte) -> 100,          // TokenAccessCostIncrease
      (6: Byte) -> 2000,         // InputCostIncrease
      (7: Byte) -> 100,          // DataInputCostIncrease
      (8: Byte) -> 100,          // OutputCostIncrease
      (123: Byte) -> 4           // BlockVersion
    )
    YoloLaunchParameters.proposedUpdate shouldBe ErgoValidationSettingsUpdate(Seq[Short](215, 409), Seq.empty)

    // within the voting bounds of the parameter
    YoloLaunchParameters.storageFeeFactor should be >= Parameters.StorageFeeFactorMin
    YoloLaunchParameters.storageFeeFactor should be <= Parameters.StorageFeeFactorMax
    // on the voting step grid, so repeated votes land exactly on the bounds and cannot pass them
    YoloLaunchParameters.storageFeeFactor % Parameters.StorageFeeFactorStep shouldBe 0

    // upstream testnet launch parameters are unchanged
    TestnetLaunchParameters.storageFeeFactor shouldBe Parameters.StorageFeeFactorDefault
    TestnetLaunchParameters.storageFeeFactor shouldBe 1250000
    TestnetLaunchParameters.maxBlockCost shouldBe Parameters.MaxBlockCostDefault
    TestnetLaunchParameters.maxBlockCost shouldBe 1000000
    MainnetLaunchParameters.maxBlockCost shouldBe 1000000
  }

  property("an absent yolo section reads as the switch off") {
    val cfg = ConfigFactory.parseString("y { }")
    cfg.as[YoloSettings]("y") shouldBe YoloSettings.disabled
  }

}
