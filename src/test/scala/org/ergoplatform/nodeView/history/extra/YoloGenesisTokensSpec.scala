package org.ergoplatform.nodeView.history.extra

import akka.actor.{ActorSystem, Props}
import akka.testkit.TestProbe
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.history.extra.ExtraIndexer.ReceivableMessages.StartExtraIndexer
import org.ergoplatform.nodeView.history.extra.IndexedTokenSerializer.uniqueId
import org.ergoplatform.nodeView.state.ErgoState
import org.ergoplatform.settings.{Args, ErgoSettings, ErgoSettingsReader, NetworkType}
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.wallet.utils.TestFileUtils
import scorex.util.{ModifierId, bytesToId}

import java.io.File
import scala.concurrent.Await
import scala.concurrent.duration._

/**
  * The YOLO genesis NFTs in the extra index: they exist only in genesis boxes and are never minted in a transaction,
  * so the indexer writes a token row for each when it indexes the genesis boxes, together with block 1
  * (`ExtraIndexer.genesisToken`). The rows themselves are checked in `ExtraIndexerSpecification`.
  */
class YoloGenesisTokensSpec extends ErgoCorePropertyTest with TestFileUtils {
  import org.ergoplatform.utils.ErgoNodeTestConstants.settings

  private lazy val devnetPath: String =
    new File(getClass.getClassLoader.getResource("yolo-devnet.conf").toURI).getAbsolutePath

  // a fresh devnet node directory with the extra index on
  private def devnet: ErgoSettings = {
    val s = ErgoSettingsReader.read(Args(Some(devnetPath), Some(NetworkType.TestNet)))
    s.copy(directory = createTempDir.getAbsolutePath, nodeSettings = s.nodeSettings.copy(extraIndex = true))
  }

  private def tokenRow(history: ErgoHistory, tokenId: String): Option[IndexedToken] =
    history.getReader.typedExtraIndexById[IndexedToken](uniqueId(ModifierId @@ tokenId))

  private def genesisTokens(boxes: Seq[org.ergoplatform.ErgoBox]): Seq[IndexedToken] =
    boxes.zipWithIndex.flatMap { case (box, number) =>
      val iEb = new IndexedErgoBox(1, None, None, None, box, number)
      (0 until box.additionalTokens.length).map(ExtraIndexer.genesisToken(iEb, _))
    }

  property("the three genesis NFTs as tokens: amount 1, created by their genesis box; none on Ergo") {
    val s = devnet
    val e = s.chainSettings.yolo.emission
    val boxes = ErgoState.genesisBoxes(s.chainSettings)
    val tokens = genesisTokens(boxes)
    tokens.map(_.tokenId) shouldBe Seq(e.emissionNftId, e.treasuryNftId, e.lpNftId)
    tokens.map(_.boxId) shouldBe boxes.take(3).map(b => Some(bytesToId(b.id)))
    tokens.foreach { t =>
      t.amount shouldBe Some(1L)
      t.name shouldBe Some("")
      t.description shouldBe Some("")
      t.decimals shouldBe Some(0)
      t.boxes shouldBe empty
    }
    genesisTokens(ErgoState.genesisBoxes(settings.chainSettings)) shouldBe empty
  }

  property("the indexer actor writes no token row before it indexes block 1") {
    val system = ActorSystem("YoloGenesisTokensSpec")
    try {
      val s = devnet
      val boxes = ErgoState.genesisBoxes(s.chainSettings)
      val history = ErgoHistory.readOrGenerate(s)(null)
      IndexerState.fromHistory(history).indexedHeight shouldBe 0
      // the indexer calls caughtUpHook once it has finished its pass and waits, caught up, for the next block;
      // nothing is written after it until a block is applied, so the absence checks below are not a race
      val probe = TestProbe()(system)
      val indexer = system.actorOf(Props(new ExtraIndexer(s.cacheSettings, s.chainSettings.addressEncoder, None,
        false, boxes) {
        override protected def caughtUpHook(height: Int): Unit = probe.ref ! "caught up"
      }))
      indexer ! StartExtraIndexer(history, None)
      probe.expectMsg(30.seconds, "caught up")
      // the rows come with the genesis boxes, in the batch of block 1, so an index at height 0 stays empty
      genesisTokens(boxes).foreach(t => tokenRow(history, t.tokenId) shouldBe None)
      IndexerState.fromHistory(history) shouldBe IndexerState(0, 0, 0, 0, caughtUp = true)
      system.stop(indexer)
      history.closeStorage()
    } finally {
      Await.result(system.terminate(), 10.seconds)
    }
  }

}
