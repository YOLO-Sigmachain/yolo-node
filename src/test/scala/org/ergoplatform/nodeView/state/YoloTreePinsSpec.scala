package org.ergoplatform.nodeView.state

import com.typesafe.config.{ConfigFactory, ConfigResolveOptions}
import org.ergoplatform.settings.YoloEmissionSettings
import org.ergoplatform.utils.ErgoCorePropertyTest
import sigma.ast.ErgoTree

import java.io.File

/**
  * The contract identity pins (`YoloTreePins`) regenerated from the devnet trees. The trees are read from
  * yolo-devnet.conf directly (not through the checked settings read, which exits the JVM when a pin drifts), so on a
  * contract change this spec fails with the regenerated pins printed as source, ready to paste into `YoloTreePins`.
  */
class YoloTreePinsSpec extends ErgoCorePropertyTest {
  import YoloTreePins._

  private lazy val devnetFile: File = new File(getClass.getClassLoader.getResource("yolo-devnet.conf").toURI)
  private lazy val emission = ConfigFactory.parseFile(devnetFile)
    .resolve(ConfigResolveOptions.defaults().setAllowUnresolved(true))
    .getConfig("ergo.chain.yolo.emission")
  private def tree(key: String): ErgoTree = YoloEmissionSettings.parseTree(emission.getString(key)).get

  private def render(name: String, p: TreePin): String = {
    val consts = p.constants.grouped(4).map(_.map(c => "\"" + c + "\"").mkString(", ")).mkString(",\n    ")
    s"""val $name: TreePin = TreePin("${p.role}", 0x${f"${p.header}%02x"}, "${p.templateHash}", IndexedSeq(
       |    $consts),
       |  ${p.idIndex}, Seq(${p.keyIndices.mkString(", ")}))""".stripMargin
  }

  property("the pins regenerated from the devnet trees equal YoloTreePins") {
    val regenerated = Seq(
      ("Emission", derive(Emission.role, tree("emissionTreeHex"), Emission.idIndex, Emission.keyIndices), Emission),
      ("Accumulation", derive(Accumulation.role, tree("treasuryAccumulationTreeHex"), Accumulation.idIndex,
        Accumulation.keyIndices), Accumulation),
      ("TreasuryFund", derive(TreasuryFund.role, tree("treasuryFundTreeHex"), TreasuryFund.idIndex,
        TreasuryFund.keyIndices), TreasuryFund),
      ("LpFund", derive(LpFund.role, tree("lpFundTreeHex"), LpFund.idIndex, LpFund.keyIndices), LpFund))
    val source = regenerated.map { case (name, p, _) => render(name, p) }.mkString("\n\n")
    info(s"regenerated pins:\n$source")
    withClue(s"the devnet trees no longer match YoloTreePins; regenerated pins:\n$source\n") {
      regenerated.foreach { case (_, derived, pinned) => derived shouldBe pinned }
    }
    // the treasury and LP accumulation trees are one contract, compiled with different ids
    derive(Accumulation.role, tree("lpAccumulationTreeHex"), Accumulation.idIndex, Accumulation.keyIndices) shouldBe
      Accumulation
  }

}
