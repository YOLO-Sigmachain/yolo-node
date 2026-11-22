package org.ergoplatform.mining

import org.ergoplatform.consensus.ProgressInfo
import org.ergoplatform.mining.difficulty.DifficultySerializer
import org.ergoplatform.modifiers.history.BlockTransactions
import org.ergoplatform.modifiers.history.extension.{Extension, ExtensionCandidate}
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.history.popow.NipopowAlgos
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.modifiers.{BlockSection, ErgoFullBlock, NonHeaderBlockSection}
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.nodeView.history.extra.{ExtraIndex, ExtraIndexer, IndexedErgoBox, NumericBoxIndex, StorageRentBox}
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.state.UtxoState
import org.ergoplatform.settings.Constants.{FalseTree, TrueTree}
import org.ergoplatform.settings.{ErgoSettings, ErgoValidationSettingsUpdate, YoloSettings}
import org.ergoplatform.utils.{ErgoCorePropertyTest, StorageRentTestHelpers}
import org.ergoplatform.{ErgoBox, Input}
import scorex.util.{ModifierId, bytesToId}
import sigma.interpreter.ProverResult

import java.nio.ByteBuffer
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

/**
  * Candidate assembly under EIP-0052 on miner attestation of rent-claim transactions (rule
  * `bsStorageRentAttestation`): `CandidateGenerator.createCandidate` adds the attestation field, over the rent-claim
  * transactions of the candidate in block order, to the candidate's extension from block version 5 on (from genesis
  * with the YOLO switch on), and adds no field when the candidate has no rent claim or the rule does not apply. The
  * generated candidate, proven with the test PoW scheme, is applied as a full block to the UTXO state it was built on.
  *
  * Named apart from `CandidateGeneratorStorageRentSpec`, which covers the v6.0.8 storage rent collection wiring.
  */
class CandidateGeneratorRentAttestationSpec extends ErgoCorePropertyTest with StorageRentTestHelpers {

  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.ErgoNodeTestConstants.genesisEmissionBox

  /** Height of the best block; the candidate is for nextHeight */
  private val tip: Int = StoragePeriod + 2001
  private val nextHeight: Int = tip + 1

  /** An empty history database, for the extra-index reads `createCandidate` makes */
  private def emptyHistoryStorage(s: ErgoSettings): HistoryStorage =
    HistoryStorage(s.copy(directory = createTempDir.getAbsolutePath))

  /**
    * History reader with `parent` as the best full block, as `createCandidate` reads it: the best full block's
    * header, its extension (interlinks of a chain whose genesis is `genesisId`), and the required difficulty.
    * Extra-index reads (the storage rent collection of `createCandidate`) go to `storage`, a real history database;
    * the full block height is the parent's.
    */
  private def historyWithBest(parent: Header,
                              anyTx: ErgoTransaction,
                              s: ErgoSettings,
                              storage: HistoryStorage): ErgoHistoryReader = {
    val genesisId: ModifierId = bytesToId(Array.fill(32)(9: Byte))
    val parentExtension = Extension(parent.id, NipopowAlgos.packInterlinks(Seq(genesisId)))
    val parentBlock = ErgoFullBlock(parent, BlockTransactions(parent.id, parent.version, Seq(anyTx)), parentExtension, None)
    new ErgoHistoryReader {
      override val historyStorage: HistoryStorage = storage
      override protected val settings: ErgoSettings = s
      override protected def requireProofs: Boolean = false
      override protected def process(m: NonHeaderBlockSection): Try[ProgressInfo[BlockSection]] = ???
      override protected def validate(m: NonHeaderBlockSection): Try[Unit] = ???
      override val powScheme: AutolykosPowScheme = s.chainSettings.powScheme

      override def bestFullBlockOpt: Option[ErgoFullBlock] = Some(parentBlock)
      override def fullBlockHeight: Int = parent.height
      override def typedModifierById[T <: BlockSection : ClassTag](id: ModifierId): Option[T] =
        Some(parentExtension).collect { case e: T => e }
      override def requiredDifficultyAfter(parent: Header): BigInt =
        DifficultySerializer.decodeCompactBits(s.chainSettings.initialNBits)
    }
  }

  /** Fresh UTXO state at `tip` with the genesis emission box and `boxes`, at the given block version */
  private def stateAtTip(version: Byte, boxes: Seq[ErgoBox]): UtxoState = {
    val ctx = stateContext(tip, version, rentSettings, validationSettingsNoIl)
    val us = utxoStateAt(genesisEmissionBox +: boxes, Some(genesisEmissionBox), ctx, rentSettings)
    val upcoming = us.stateContext.simplifiedUpcoming()
    upcoming.currentHeight shouldBe nextHeight
    upcoming.blockVersion shouldBe version
    us
  }

  /** Proves `block` with the test PoW scheme, checking height, voting epoch and extension commitment */
  private def prove(block: CandidateBlock, s: ErgoSettings): ErgoFullBlock = {
    val fb = powScheme.proveCandidate(block, defaultMinerSecretNumber).get
    fb.header.height shouldBe nextHeight
    fb.header.votingStarts(s.chainSettings.voting.votingLength) shouldBe false
    java.util.Arrays.equals(fb.header.extensionRoot, block.extension.digest) shouldBe true
    fb
  }

  /**
    * Candidate generated by `CandidateGenerator.createCandidate` on `us`, with the emission transaction and `pool`
    * as mempool transactions, and the full block obtained by proving it with the test PoW scheme
    */
  private def generate(us: UtxoState, pool: Seq[ErgoTransaction]): (CandidateBlock, ErgoFullBlock) = {
    val emission = CandidateGenerator.collectEmission(us, defaultMinerPk, us.stateContext).get
    val history = historyWithBest(us.stateContext.lastHeaderOpt.get, emission, rentSettings,
      emptyHistoryStorage(rentSettings))
    val (candidate, _) = CandidateGenerator.createCandidate(defaultMinerPk, history, ErgoValidationSettingsUpdate.empty,
      us, pool.map(tx => UnconfirmedTransaction(tx, None)), Some(emission), Seq.empty, rentSettings).get
    val block = candidate.candidateBlock
    block.transactions.head shouldBe emission
    block -> prove(block, rentSettings)
  }

  property("candidateAttests: at block version 5, the candidate's extension carries the attestation field over the " +
    "rent claims collected, wherever collectTxs placed them, and the proven candidate applies to the state; with " +
    "no claim collected there is no field; at block version 4 there is no field") {
    val plain = boxAt(TrueTree, tip - 10, seed = 31)
    // `false` script: valid only through the rent branch
    val expired = boxAt(FalseTree, nextHeight - StoragePeriod - 50, seed = 32)
    // StoragePeriod - 1 blocks old for a block at tip (when it was admitted, the tip was tip - 1),
    // exactly StoragePeriod old at nextHeight
    val crossing = boxAt(TrueTree, nextHeight - StoragePeriod, seed = 33)
    val boxes = Seq(plain, expired, crossing)

    val plainTx = ErgoTransaction(IndexedSeq(Input(plain.id, ProverResult.empty)), IndexedSeq(recreated(plain, nextHeight)))
    val claimTx = rentShapedTx(expired, nextHeight)
    val crossingTx = rentShapedTx(crossing, nextHeight)

    ErgoTransaction.hasStorageRentClaim(crossingTx, IndexedSeq(crossing), tip) shouldBe false
    ErgoTransaction.hasStorageRentClaim(crossingTx, IndexedSeq(crossing), nextHeight) shouldBe true
    ErgoTransaction.hasStorageRentClaim(claimTx, IndexedSeq(expired), nextHeight) shouldBe true
    ErgoTransaction.hasStorageRentClaim(plainTx, IndexedSeq(plain), nextHeight) shouldBe false

    // (1) version 5, claims offered after a non-claim: all collected in pool order, attested in block order
    val us1 = stateAtTip(RentAttestationVersion, boxes)
    val (block1, fb1) = generate(us1, Seq(plainTx, claimTx, crossingTx))
    block1.version shouldBe RentAttestationVersion
    block1.transactions.map(_.id) shouldBe Seq(block1.transactions.head.id, plainTx.id, claimTx.id, crossingTx.id)
    block1.transactions.indexOf(claimTx) shouldBe 2
    block1.transactions.indexOf(crossingTx) shouldBe 3
    val values1 = attestationValues(block1.extension)
    values1.length shouldBe 1
    java.util.Arrays.equals(values1.head, attestationDigest(claimTx, crossingTx)) shouldBe true
    // the interlinks from the parent's extension are kept next to the field
    block1.extension.fields.exists(_._1.head == Extension.InterlinksVectorPrefix) shouldBe true
    // the proven candidate is a valid block at version 5, rule 308 included
    us1.applyModifier(fb1, None)(_ => ()) shouldBe a[Success[_]]

    // (2) version 5, a claim offered first: it follows the emission transaction, and is attested alone
    val us2 = stateAtTip(RentAttestationVersion, boxes)
    val (block2, fb2) = generate(us2, Seq(claimTx, plainTx))
    block2.transactions.tail shouldBe Seq(claimTx, plainTx)
    val values2 = attestationValues(block2.extension)
    values2.length shouldBe 1
    java.util.Arrays.equals(values2.head, attestationDigest(claimTx)) shouldBe true
    us2.applyModifier(fb2, None)(_ => ()) shouldBe a[Success[_]]

    // (3) version 5, no claim collected: no field
    val us3 = stateAtTip(RentAttestationVersion, boxes)
    val (block3, fb3) = generate(us3, Seq(plainTx))
    block3.transactions.tail shouldBe Seq(plainTx)
    attestationValues(block3.extension) shouldBe empty
    us3.applyModifier(fb3, None)(_ => ()) shouldBe a[Success[_]]

    // (4) version 4, claims collected: no field (rule 308 is not enforced there), and the block applies
    val us4 = stateAtTip(Header.Interpreter60Version, boxes)
    val (block4, fb4) = generate(us4, Seq(plainTx, claimTx, crossingTx))
    block4.version shouldBe Header.Interpreter60Version
    block4.transactions.tail shouldBe Seq(plainTx, claimTx, crossingTx)
    attestationValues(block4.extension) shouldBe empty
    us4.applyModifier(fb4, None)(_ => ()) shouldBe a[Success[_]]
  }

  property("yoloSelfCollection: with the YOLO switch on and storageRentCollection, at block version 4, the " +
    "candidate's extension attests to the node's own rent claim (built by StorageRentClaimBuilder from the extra " +
    "index) and to a mempool claim, in block order, and the proven candidate applies; without the field the same " +
    "block is rejected by rule 308") {
    val yoloSettings: ErgoSettings = rentSettings.copy(
      chainSettings = rentSettings.chainSettings.copy(yolo = YoloSettings(enabled = true)),
      nodeSettings = rentSettings.nodeSettings.copy(storageRentCollection = true, extraIndex = true))
    val version = Header.Interpreter60Version
    version < Header.StorageRentAttestationVersion shouldBe true
    yoloSettings.chainSettings.storageRentAttestationRequired(version) shouldBe true
    rentSettings.chainSettings.storageRentAttestationRequired(version) shouldBe false

    val plain = boxAt(TrueTree, tip - 10, seed = 41)
    // swept by the node itself: indexed in the extra index below, with its rent eligibility entry
    val ownExpired = boxAt(TrueTree, nextHeight - StoragePeriod - 20, seed = 42)
    // claimed by a mempool transaction; `false` script, so valid only through the rent branch
    val poolExpired = boxAt(FalseTree, nextHeight - StoragePeriod - 30, seed = 43)
    val boxes = Seq(plain, ownExpired, poolExpired)

    val plainTx = ErgoTransaction(IndexedSeq(Input(plain.id, ProverResult.empty)), IndexedSeq(recreated(plain, nextHeight)))
    val poolClaim = rentShapedTx(poolExpired, nextHeight)

    // One context for both states, so that both have the same last header (the candidate's parent).
    // No emission box: the YOLO emission inputs are not set in these settings
    val ctx = stateContext(tip, version, yoloSettings, validationSettingsNoIl)
    val us = utxoStateAt(boxes, None, ctx, yoloSettings)
    val usControl = utxoStateAt(boxes, None, ctx, yoloSettings)
    us.stateContext.simplifiedUpcoming().currentHeight shouldBe nextHeight
    us.stateContext.blockVersion shouldBe version

    // extra index synced at the tip, holding `ownExpired` and its storage rent eligibility entry
    val storage = emptyHistoryStorage(yoloSettings)
    val globalIndex = 7L
    val iEb = new IndexedErgoBox(ownExpired.creationHeight, None, None, None, ownExpired, globalIndex)
    storage.insertExtra(
      Array(ExtraIndexer.IndexedHeightKey -> ByteBuffer.allocate(4).putInt(tip).array),
      Array[ExtraIndex](iEb, NumericBoxIndex(globalIndex, bytesToId(ownExpired.id)), StorageRentBox(iEb)))
    val history = historyWithBest(ctx.lastHeaderOpt.get, plainTx, yoloSettings, storage)

    val (candidate, _) = CandidateGenerator.createCandidate(defaultMinerPk, history, ErgoValidationSettingsUpdate.empty,
      us, Seq(plainTx, poolClaim).map(tx => UnconfirmedTransaction(tx, None)), None, Seq.empty, yoloSettings).get
    val block = candidate.candidateBlock
    block.version shouldBe version

    // the node's own claim: not from the pool, spends `ownExpired`, and sits before the pool transactions
    val ownClaim = block.transactions.find(_.inputs.exists(i => java.util.Arrays.equals(i.boxId, ownExpired.id))).get
    ownClaim.id should not be plainTx.id
    ownClaim.id should not be poolClaim.id
    ErgoTransaction.hasStorageRentClaim(ownClaim, IndexedSeq(ownExpired), nextHeight) shouldBe true
    block.transactions shouldBe Seq(ownClaim, plainTx, poolClaim)

    val values = attestationValues(block.extension)
    values.length shouldBe 1
    java.util.Arrays.equals(values.head, attestationDigest(ownClaim, poolClaim)) shouldBe true
    java.util.Arrays.equals(values.head,
      CandidateGenerator.storageRentAttestation(block.transactions, us, us.stateContext.simplifiedUpcoming())
        .fields.head._2) shouldBe true

    // control: the same block without the field is rejected by rule 308
    val unattested = block.copy(extension =
      ExtensionCandidate(block.extension.fields.filterNot(f => java.util.Arrays.equals(f._1, AttestationKey))))
    usControl.applyModifier(prove(unattested, yoloSettings), None)(_ => ()) match {
      case Failure(e) => e.getMessage should include("Rent-claim transactions must be attested")
      case Success(_) => fail("block with unattested rent claims applied on YOLO at block version 4")
    }

    // the candidate as generated is a valid block on YOLO, rule 308 included
    val applied = us.applyModifier(prove(block, yoloSettings), None)(_ => ())
    applied shouldBe a[Success[_]]
    applied.get.boxById(ownExpired.id) shouldBe None
    applied.get.boxById(poolExpired.id) shouldBe None
  }

}
