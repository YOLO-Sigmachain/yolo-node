package org.ergoplatform.nodeView.mempool

import com.google.common.primitives.Ints
import org.ergoplatform.ErgoBox
import org.ergoplatform.ErgoBox.BoxId
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import scorex.db.{LDBFactory, LDBKVStore}
import scorex.util.{ModifierId, ScorexLogging, bytesToId, idToBytes}
import sigma.ast.{ErgoTree, SigmaPropConstant}
import sigma.data.{CSigmaProp, ProveDlog}

import scala.util.Try

/**
  * YOLO switch on only: boxes refused at mempool admission, by box id, with the height at which each was
  * refused. A box is added when assembly evicts a transaction with a record that spends it as a cost liar or as a
  * failure (`YoloAssemblyPolicy`); data inputs are never added, nor the inputs of the emission tx, the rent claim or
  * prioritized transactions. A box leaves when it is spent in an applied block or, if it is not in the UTXO set, after
  * `AgeLimit` blocks. There is no other time limit. An entry `AgeLimit` blocks old whose box is still in the UTXO set
  * is re-stamped at the current height, so each entry is looked up at most once per `AgeLimit` blocks.
  *
  * Sponsor rule (`admits`): a transaction that spends a refused box is still admitted if it also spends a sponsor: a
  * box that is not refused, whose script is a plain P2PK (`isPlainP2PK`) and whose spending proof is not empty, that
  * is a plain key-signed box. A P2PK box cannot be spent without a valid signature, and the transaction is fully
  * validated after this check, so junk proof bytes on a box whose script anyone can satisfy make no sponsor. When such
  * a transaction is caught, all its direct inputs are refused, the sponsor included. So an honest user who spends a
  * refused shared box (a DEX pool box, an accumulation box) with a plain key-signed box of its own is admitted. A bot
  * whose transactions have no such input must add one while a box it spends is refused.
  *
  * Immutable; persisted by the node view holder through `YoloRefusedBoxesStore`.
  */
final class YoloRefusedBoxes private(val entries: Map[ModifierId, Int]) {

  def size: Int = entries.size

  def isEmpty: Boolean = entries.isEmpty

  def contains(boxId: ModifierId): Boolean = entries.contains(boxId)

  /**
    * Whether `tx` passes the sponsor rule: it spends no refused box, or it also spends a sponsor. `boxOf` resolves an
    * input's box (from the state and the pool's outputs); an input whose box it cannot resolve is not a sponsor.
    */
  def admits(tx: ErgoTransaction, boxOf: BoxId => Option[ErgoBox]): Boolean = entries.isEmpty || {
    val (refused, free) = tx.inputs.partition(i => entries.contains(bytesToId(i.boxId)))
    refused.isEmpty || free.exists { i =>
      i.spendingProof.proof.nonEmpty && boxOf(i.boxId).exists(b => YoloRefusedBoxes.isPlainP2PK(b.ergoTree))
    }
  }

  /** The refused boxes `tx` spends */
  def refusedInputsOf(tx: ErgoTransaction): Seq[ModifierId] =
    tx.inputs.map(i => bytesToId(i.boxId)).filter(entries.contains)

  def added(boxIds: Seq[ModifierId], height: Int): YoloRefusedBoxes =
    if (boxIds.isEmpty) this else new YoloRefusedBoxes(entries ++ boxIds.map(_ -> height))

  def removed(boxIds: Iterable[ModifierId]): YoloRefusedBoxes =
    if (boxIds.isEmpty || entries.isEmpty) this else new YoloRefusedBoxes(entries -- boxIds)

  /**
    * The set at `height`: entries refused at least `AgeLimit` blocks before are dropped if their box is not in the UTXO
    * set, and re-stamped at `height` if it is (so each is looked up at most once per `AgeLimit` blocks). This same
    * instance if no entry is that old.
    */
  def aged(height: Int, inUtxoSet: ModifierId => Boolean): YoloRefusedBoxes = {
    val old = entries.collect { case (id, h) if height - h >= YoloRefusedBoxes.AgeLimit => id }
    if (old.isEmpty) {
      this
    } else {
      val (unspent, gone) = old.partition(inUtxoSet)
      new YoloRefusedBoxes(entries -- gone ++ unspent.map(_ -> height))
    }
  }
}

object YoloRefusedBoxes {

  /** A = 2,880 blocks (one day): how long an entry for a box that is not in the UTXO set is kept */
  val AgeLimit: Int = 2880

  val empty: YoloRefusedBoxes = new YoloRefusedBoxes(Map.empty)

  def apply(entries: Map[ModifierId, Int]): YoloRefusedBoxes = new YoloRefusedBoxes(entries)

  /** Whether `tree` is a plain P2PK: a single `ProveDlog` sigma proposition, the standard pay-to-public-key form */
  def isPlainP2PK(tree: ErgoTree): Boolean = tree.root match {
    case Right(SigmaPropConstant(CSigmaProp(_: ProveDlog))) => true
    case _ => false
  }

  /** The admission decline for a transaction that spends a refused box without a sponsor */
  class Refusal(message: String) extends Exception(message)
}

/**
  * Disk store of `YoloRefusedBoxes`, a LevelDB key-value store: box id to the height it was refused at.
  * Written by the node view holder whenever its pool's set changes; read once at start.
  */
final class YoloRefusedBoxesStore(store: LDBKVStore) extends ScorexLogging {

  def load(): YoloRefusedBoxes = YoloRefusedBoxes(store.getAll.map { case (k, v) => bytesToId(k) -> Ints.fromByteArray(v) }.toMap)

  /** Writes the difference between `before` and `after` */
  def update(before: YoloRefusedBoxes, after: YoloRefusedBoxes): Unit = if (before ne after) {
    val toInsert = after.entries.filter { case (id, h) => !before.entries.get(id).contains(h) }.toArray
    val toRemove = before.entries.keys.filterNot(after.entries.contains).map(idToBytes).toArray
    if (toInsert.nonEmpty || toRemove.nonEmpty) {
      store.update(toInsert.map(e => idToBytes(e._1)), toInsert.map(e => Ints.toByteArray(e._2)), toRemove)
        .failed.foreach(e => log.error("Failed to write the YOLO refused boxes", e))
    }
  }

  def close(): Unit = Try(store.close())
}

object YoloRefusedBoxesStore extends ScorexLogging {

  /** Opens the store at `dir`, or None (logged) if it cannot be opened: the set is then kept in memory only */
  def open(dir: String): Option[YoloRefusedBoxesStore] =
    Try(new YoloRefusedBoxesStore(LDBFactory.createKvDb(dir))).fold({ e =>
      log.error(s"Cannot open the YOLO refused boxes store at $dir, keeping the set in memory only", e)
      None
    }, Some(_))
}
