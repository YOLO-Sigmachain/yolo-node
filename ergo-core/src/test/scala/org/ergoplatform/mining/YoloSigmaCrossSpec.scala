package org.ergoplatform.mining

import com.google.common.primitives.Ints
import org.ergoplatform.mining.difficulty.DifficultySerializer
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32
import sigma.pow.Autolykos2PowValidation

/**
  * Cross-check of the YOLO `pow_height = height / d` mapping against sigma's
  * independent Autolykos 2 implementation (`sigma.pow.Autolykos2PowValidation`).
  *
  * For headers mined at d = 4, sigma's hit computed with `height / 4` as table seed and `calcN(height / 4)`
  * equals the node's hit, so the node's mapping is the one sigma would compute given the pow height.
  *
  * Sigma's own `Header.checkPow` (the ErgoScript `Header.checkPow` method) uses the real header height,
  * so it returns `false` for YOLO headers. This is the known behaviour and is pinned here: contracts on
  * YOLO must not use `Header.checkPow`; they must use `Global.powHit` with `height / 4` and
  * `calcN(height / 4)` instead.
  */
class YoloSigmaCrossSpec extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.generators.ErgoCoreGenerators._

  private val d4 = new AutolykosPowScheme(32, 26, 4)

  /** Difficulty of the mined headers: about 1 nonce in 4096 is a solution. */
  private val nBits: Long = DifficultySerializer.encodeCompactBits(BigInt(4096))

  property("at d = 4, sigma's hit with height / 4 equals the node hit, and sigma Header.checkPow is false") {
    Seq(5, 1001, 2457605).foreach { h =>
      withClue(s"height $h: ") {
        // fixed generator seed, so the mined header (and so the checkPow result) is deterministic
        val parent = defaultHeaderGen.pureApply(Gen.Parameters.default, Seed(h.toLong))
          .copy(height = h - 1, version = Header.Interpreter60Version)
        val hdr = d4.prove(Some(parent), Header.Interpreter60Version, nBits, ADDigest @@ Array.fill(33)(1: Byte),
          Digest32 @@ Array.fill(32)(2: Byte), Digest32 @@ Array.fill(32)(3: Byte), 1700000000000L,
          Digest32 @@ Array.fill(32)(4: Byte), Array.fill(3)(0: Byte), BigInt(12345), 0L, 10000000L)
          .getOrElse(fail("no solution found"))
        hdr.height shouldBe h
        d4.validate(hdr) shouldBe 'success

        val powHeight = h / 4
        val sigmaHit = Autolykos2PowValidation.hitForVersion2ForMessageWithChecks(32, d4.msgByHeader(hdr),
          hdr.powSolution.n, Ints.toByteArray(powHeight), Autolykos2PowValidation.calcN(powHeight))
        d4.hitForVersion2(hdr) shouldBe sigmaHit
        d4.calcN(hdr) shouldBe Autolykos2PowValidation.calcN(powHeight)

        // Known behaviour: sigma's checkPow uses the real height, so it rejects YOLO headers.
        // Contracts must use Global.powHit (with height / 4 and calcN(height / 4)), not Header.checkPow.
        Header.toSigma(hdr).checkPow shouldBe false
      }
    }
  }

}
