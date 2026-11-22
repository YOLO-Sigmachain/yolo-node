package org.ergoplatform.nodeView.wallet

import com.google.common.hash.{BloomFilter, Funnels}
import org.ergoplatform.mining.YoloEmissionTx
import org.ergoplatform.sdk.wallet.secrets.ExtendedPublicKey
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.{ErgoAddressEncoder, ErgoTreePredef, P2PKAddress}
import scorex.util.encode.Base16
import sigma.ast.ErgoTree
import sigmastate.eval._

import scala.util.Try

/**
  * Fields of WalletVars which are potentially costly to compute if there are many keys in the wallet
  */
final case class WalletCache(publicKeyAddresses: Seq[P2PKAddress],
                             trackedPubKeys: Seq[ExtendedPublicKey],
                             trackedBytes: Seq[Array[Byte]],
                             scriptsFilter: BloomFilter[Array[Byte]])(implicit val settings: ErgoSettings) {

  implicit val addressEncoder: ErgoAddressEncoder = settings.addressEncoder

  // Mining rewards are being tracked only if mining = true in config
  private val miningScripts: Seq[ErgoTree] = {
    if (settings.nodeSettings.mining) {
      WalletCache.miningScripts(trackedPubKeys, settings)
    } else {
      Seq.empty
    }
  }

  val miningScriptsBytes: Seq[Array[Byte]] = miningScripts.map(_.bytes)

  /**
    * Maturity delay of each mining script, by hex of the script bytes (`WalletCache.miningScriptDelays`); empty if
    * mining rewards are not tracked.
    */
  val miningScriptDelays: Map[String, Int] = {
    if (settings.nodeSettings.mining) {
      WalletCache.miningScriptDelays(trackedPubKeys, settings)
    } else {
      Map.empty
    }
  }

  def withNewPubkey(newPk: ExtendedPublicKey): Try[WalletCache] = Try {
    val updAddresses: Seq[P2PKAddress] = publicKeyAddresses :+ P2PKAddress(newPk.key)
    val updTrackedPubKeys: Seq[ExtendedPublicKey] = trackedPubKeys :+ newPk
    val newPkBytes = newPk.key.propBytes.toArray
    val updTrackedBytes: Seq[Array[Byte]] = trackedBytes :+ newPkBytes

    // update filter
    scriptsFilter.put(newPkBytes)

    WalletCache(updAddresses, updTrackedPubKeys, updTrackedBytes, scriptsFilter)
  }

}

object WalletCache {

  def miningScripts(trackedPubKeys: Seq[ExtendedPublicKey],
                    settings: ErgoSettings): Seq[ErgoTree] = {
    trackedPubKeys.map { pk =>
      ErgoTreePredef.rewardOutputScript(settings.miningRewardDelay, pk.key)
    } ++ yoloCoinbaseScripts(trackedPubKeys, settings)
  }

  // YOLO: the coinbase miner output is at its own delay (YoloEmissionTx.CoinbaseMinerRewardDelay), the fee
  // output at miningRewardDelay; with the switch off there is no coinbase script
  private def yoloCoinbaseScripts(trackedPubKeys: Seq[ExtendedPublicKey], settings: ErgoSettings): Seq[ErgoTree] =
    if (settings.chainSettings.yolo.enabled) trackedPubKeys.map(pk => YoloEmissionTx.minerOutputScript(pk.key)) else Seq.empty

  /**
    * Maturity delay of each script of `miningScripts`, by hex of the script bytes: `miningRewardDelay` for the fee
    * script and, with the YOLO switch on, `YoloEmissionTx.CoinbaseMinerRewardDelay` for the coinbase script.
    */
  def miningScriptDelays(trackedPubKeys: Seq[ExtendedPublicKey], settings: ErgoSettings): Map[String, Int] = {
    val fee = trackedPubKeys.map { pk =>
      Base16.encode(ErgoTreePredef.rewardOutputScript(settings.miningRewardDelay, pk.key).bytes) -> settings.miningRewardDelay
    }
    val coinbase = yoloCoinbaseScripts(trackedPubKeys, settings).map { s =>
      Base16.encode(s.bytes) -> YoloEmissionTx.CoinbaseMinerRewardDelay
    }
    (fee ++ coinbase).toMap
  }

  /**
    * Create empty bloom filter
    * @return a Bloom filter instance
    */
  def emptyFilter(expectedKeys: Int): BloomFilter[Array[Byte]] = {
    val falsePositiveRate = 0.001
    BloomFilter.create[Array[Byte]](Funnels.byteArrayFunnel(), expectedKeys, falsePositiveRate)
  }

  /**
    * Constructing a Bloom filter for scanning wallet-related scripts efficiently
    */
  def createScriptsFilter(trackedBytes: Seq[Array[Byte]],
                          miningScriptsBytes: Seq[Array[Byte]],
                          walletProfile: WalletProfile): BloomFilter[Array[Byte]] = {
    val f = emptyFilter(walletProfile.scriptsFilterSize)
    trackedBytes.foreach(bs => f.put(bs))
    miningScriptsBytes.foreach(msb => f.put(msb))
    f
  }

  private def trackedBytes(trackedPubKeys: Seq[ExtendedPublicKey]): Seq[Array[Byte]] =
    trackedPubKeys.map(_.key.propBytes.toArray)

  private def publicKeyAddresses(trackedPubKeys: Seq[ExtendedPublicKey],
                                 addressEncoder: ErgoAddressEncoder): Seq[P2PKAddress] = {
    // Remove master key from visible addresses if following-up keys are from EIP-3 range
    val pks = if (trackedPubKeys.size > 1
      && trackedPubKeys.head.path.isMaster
      && trackedPubKeys(1).path.isEip3) {
      // Skip master key
      trackedPubKeys.tail
    } else {
      trackedPubKeys
    }
    pks.map(pk => P2PKAddress(pk.key)(addressEncoder))
  }

  def apply(trackedPubKeys: Seq[ExtendedPublicKey], settings: ErgoSettings): WalletCache = {
    val tbs = trackedBytes(trackedPubKeys)
    val msBytes = miningScripts(trackedPubKeys, settings).map(_.bytes)
    val scriptsFilter = createScriptsFilter(tbs, msBytes, settings.walletSettings.walletProfile)
    val pka = publicKeyAddresses(trackedPubKeys, settings.addressEncoder)

    WalletCache(pka, trackedPubKeys, tbs, scriptsFilter)(settings)
  }

}
