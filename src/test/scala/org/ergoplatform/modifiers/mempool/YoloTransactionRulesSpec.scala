package org.ergoplatform.modifiers.mempool

import org.ergoplatform.settings.{Constants, ErgoSettings, ValidationRules, YoloSettings}
import org.ergoplatform.utils.{ErgoCorePropertyTest, ErgoStateContextHelpers}
import org.ergoplatform.wallet.interpreter.ErgoInterpreter
import org.ergoplatform.{DataInput, ErgoBox, Input}
import org.scalacheck.Gen
import scorex.crypto.authds.ADKey
import scorex.util.ModifierId
import sigma.ast.ShortConstant
import sigma.interpreter.{ContextExtension, ProverResult}

/**
  * Rules 110 (`txDataInputsUnique`) and 125 (`txRentDistinctOutputs`) are active from genesis when the
  * YOLO switch is on, and keep the upstream 1,885,000 activation when it is off.
  */
class YoloTransactionRulesSpec extends ErgoCorePropertyTest with ErgoStateContextHelpers {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.generators.{ErgoNodeTransactionGenerators => NodeTransactionGenerators}
  import org.ergoplatform.utils.generators.ErgoNodeTransactionGenerators._

  private implicit val verifier: ErgoInterpreter = ErgoInterpreter(parameters)

  private val yoloSettings: ErgoSettings =
    settings.copy(chainSettings = settings.chainSettings.copy(yolo = YoloSettings(enabled = true)))

  private val emptyId = ModifierId @@ ""

  private def ruleMessage(rule: Short): String =
    ValidationRules.errorMessage(rule, "", emptyId, ErgoTransaction.modifierTypeId).take(30)

  // a height well below the upstream activation height, inside the YOLO rent window
  private val lowHeight = Constants.StoragePeriod + 10

  property("rule 110: three data inputs with one box id are rejected from genesis on YOLO only") {
    val dataBoxesGen = Gen.nonEmptyListOf(NodeTransactionGenerators.ergoBoxGenNoProp).map(_.toIndexedSeq)
    forAll(validErgoTransactionGen, dataBoxesGen) { case ((from, tx), dataBoxes) =>
      def ref(b: ErgoBox): DataInput = DataInput(ADKey @@ b.id.clone())
      val thriceInputs = ref(dataBoxes.head) +: ref(dataBoxes.head) +: dataBoxes.map(ref)
      val thriceBoxes = dataBoxes.head +: dataBoxes.head +: dataBoxes
      val txThrice = ErgoTransaction(tx.inputs, thriceInputs, tx.outputCandidates)

      Seq(1, lowHeight, ErgoTransaction.DataInputsUniquenessHeight - 1).foreach { h =>
        // switch off: still valid below the upstream activation height
        txThrice.statefulValidity(from, thriceBoxes, stateContext(h, 1, settings)).isSuccess shouldBe true
        // switch on: rejected by rule 110
        val res = txThrice.statefulValidity(from, thriceBoxes, stateContext(h, 1, yoloSettings))
        res.isSuccess shouldBe false
        res.failed.get.getMessage should include(ruleMessage(ValidationRules.txDataInputsUnique))
      }

      // unique data inputs stay valid on YOLO
      val txUnique = ErgoTransaction(tx.inputs, dataBoxes.map(ref), tx.outputCandidates)
      txUnique.statefulValidity(from, dataBoxes, stateContext(1, 1, yoloSettings)).isSuccess shouldBe true
    }
  }

  property("rule 125: repeated var #127 values across inputs are rejected from genesis on YOLO only") {
    val gen = validErgoTransactionGen.suchThat(_._2.inputs.size >= 2)
    forAll(gen) { case (from, tx) =>
      def withVar127(in: Input, v: Short): Input =
        new Input(in.boxId, ProverResult(in.spendingProof.proof,
          ContextExtension(in.spendingProof.extension.values.updated(Constants.StorageIndexVarId, ShortConstant(v)))))

      val repeated = tx.copy(inputs = tx.inputs.map(withVar127(_, 0)))
      val distinct = tx.copy(inputs = tx.inputs.zipWithIndex.map { case (in, i) => withVar127(in, i.toShort) })

      // heights below the storage period, so the generated boxes are not rent-eligible and the input
      // scripts (true) decide; above it the interpreter would take the rent path for var #127
      Seq(1, Constants.StoragePeriod - 1).foreach { h =>
        // switch off: the rule is not applied below the upstream activation height
        repeated.statefulValidity(from, emptyDataBoxes, stateContext(h, 1, settings)).isSuccess shouldBe true
        // switch on: rejected by rule 125
        val res = repeated.statefulValidity(from, emptyDataBoxes, stateContext(h, 1, yoloSettings))
        res.isSuccess shouldBe false
        res.failed.get.getMessage should include(ruleMessage(ValidationRules.txRentDistinctOutputs))
        // distinct values pass on YOLO
        distinct.statefulValidity(from, emptyDataBoxes, stateContext(h, 1, yoloSettings)).isSuccess shouldBe true
      }

      // switch off, at the upstream activation height: rejected, as upstream
      val atActivation = repeated.statefulValidity(from, emptyDataBoxes,
        stateContext(ValidationRules.StorageRentDistinctOutputsActivationHeight, 1, settings))
      atActivation.isSuccess shouldBe false
      atActivation.failed.get.getMessage should include(ruleMessage(ValidationRules.txRentDistinctOutputs))
    }
  }

}
