package org.ergoplatform.mining

import com.google.common.primitives.Ints
import com.typesafe.config.ConfigFactory
import io.circe.syntax._
import net.ceedubs.ficus.Ficus._
import org.ergoplatform.mining.difficulty.DifficultySerializer
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions}
import org.ergoplatform.modifiers.history.extension.ExtensionCandidate
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.settings.PowSchemeReaders
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.scalacheck.Gen
import scorex.crypto.authds.{ADDigest, SerializedAdProof}
import sigma.pow.Autolykos2PowValidation

/**
  * YOLO `pow_height = height / d` mapping.
  *
  * With d = 1 the scheme is upstream Ergo (the upstream vectors are in `AutolykosPowSchemeSpec`).
  * With d = 4 the N schedule, the table seed `h` (validation and CPU prover) and the `h` field of the
  * external work message use height / 4, while the header message still commits to the real height.
  */
class YoloPowHeightSpec extends ErgoCorePropertyTest with PowSchemeReaders {
  import org.ergoplatform.utils.generators.ErgoCoreGenerators._

  private val upstream = new AutolykosPowScheme(32, 26)
  private val d1 = new AutolykosPowScheme(32, 26, 1)
  private val d4 = new AutolykosPowScheme(32, 26, 4)

  /** A fixed header (the upstream height-614,400 test vector) used as a deterministic template. */
  private lazy val template: Header = {
    import io.circe.parser._
    val headerJson =
      """
        |{
        |  "extensionId" : "00cce45975d87414e8bdd8146bc88815be59cd9fe37a125b5021101e05675a18",
        |  "difficulty" : "16384",
        |  "votes" : "000000",
        |  "timestamp" : 4928911477310178288,
        |  "size" : 223,
        |  "stateRoot" : "5c8c00b8403d3701557181c8df800001b6d5009e2201c6ff807d71808c00019780",
        |  "height" : 614400,
        |  "nBits" : 37748736,
        |  "version" : 2,
        |  "id" : "5603a937ec1988220fc44fb5022fb82d5565b961f005ebb55d85bd5a9e6f801f",
        |  "adProofsRoot" : "5d3f80dcff7f5e7f59007294c180808d0158d1ff6ba10000f901c7f0ef87dcff",
        |  "transactionsRoot" : "f17fffacb6ff7f7f1180d2ff7f1e24ffffe1ff937f807f0797b9ff6ebdae007e",
        |  "extensionHash" : "1480887f80007f4b01cf7f013ff1ffff564a0000b9a54f00770e807f41ff88c0",
        |  "powSolutions" : {
        |    "pk" : "03bedaee069ff4829500b3c07c4d5fe6b3ea3d3bf76c5c28c1d4dcdb1bed0ade0c",
        |    "n" : "0000000000003105"
        |   },
        |  "adProofsId" : "dec129290a763f4de41f04e87e2b661dd59758af6bdd00dd51f5d97c3a8cb9b5",
        |  "transactionsId" : "eba1dd82cf51147232e09c1f72b37c554c30f63274d5093bff36849a83472a42",
        |  "parentId" : "ac2101807f0000ca01ff0119db227f202201007f62000177a080005d440896d0"
        |}
      """.stripMargin
    Header.jsonDecoder.decodeJson(parse(headerJson).toOption.get).toOption.get
  }

  /** Difficulty of the candidates mined in this spec: about 1 nonce in 4096 is a solution. */
  private val testNBits: Long = DifficultySerializer.encodeCompactBits(BigInt(4096))

  /** Fixed secret key for the CPU prover. */
  private val sk: BigInt = BigInt("1234567890123456789012345678901234567890")

  /** Block candidate at `height` (its parent is the template at `height - 1`), with fixed contents. */
  private def candidateAt(height: Int): CandidateBlock = {
    val parent = template.copy(height = height - 1)
    CandidateBlock(
      parentOpt = Some(parent),
      version = Header.Interpreter60Version,
      nBits = testNBits,
      stateRoot = ADDigest @@ Array.fill(33)(1: Byte),
      adProofBytes = SerializedAdProof @@ Array.emptyByteArray,
      transactions = Seq.empty,
      timestamp = 1700000000000L,
      extension = ExtensionCandidate(Seq.empty),
      votes = Array.fill(3)(0: Byte))
  }

  /**
    * Mines the header of `candidate` with the CPU prover of `pow` (`prove`, the header part of
    * `proveCandidate`, as `proveBlock` requires a coinbase transaction), searching nonces from 0.
    */
  private def mine(pow: AutolykosPowScheme, candidate: CandidateBlock): Header =
    pow.prove(candidate.parentOpt, candidate.version, candidate.nBits, candidate.stateRoot,
      ADProofs.proofDigest(candidate.adProofBytes),
      BlockTransactions.transactionsRoot(candidate.transactions, candidate.version),
      candidate.timestamp, candidate.extension.digest, candidate.votes, sk, 0L, 10000000L)
      .getOrElse(fail("no solution found"))

  property("default divisor is 1 and pow height is the identity at d = 1") {
    upstream.powHeightDivisor shouldBe 1
    forAll(Gen.choose(0, Int.MaxValue)) { h =>
      upstream.powHeight(h) shouldBe h
      d1.powHeight(h) shouldBe h
      d4.powHeight(h) shouldBe h / 4
    }
  }

  property("divisor below 1 is rejected") {
    an[IllegalArgumentException] should be thrownBy new AutolykosPowScheme(32, 26, 0)
    an[IllegalArgumentException] should be thrownBy new DefaultFakePowScheme(32, 26, -4)
  }

  property("at d = 1 the scheme matches sigma's independent Autolykos 2 implementation on random headers") {
    forAll(invalidHeaderGen, Gen.choose[Byte](1, 2)) { (inHeader, ver) =>
      val h = inHeader.copy(version = ver)
      d1.calcN(h) shouldBe upstream.calcN(ver, h.height)
      d1.calcN(h) shouldBe upstream.calcN(h)
      if (ver > Header.InitialVersion) {
        d1.hitForVersion2(h) shouldBe upstream.hitForVersion2(h)
        // sigma.pow.Autolykos2PowValidation is a separate implementation, not this class
        d1.calcN(h) shouldBe Autolykos2PowValidation.calcN(h.height)
        d1.hitForVersion2(h) shouldBe Autolykos2PowValidation.hitForVersion2(Header.toSigma(h))
      }
    }
  }

  property("at d = 1 the upstream height-614,400 vector still validates") {
    d1.calcN(template) shouldBe 70464240
    d1.validate(template) shouldBe 'success
  }

  property("at d = 4 N equals upstream calcN at height / 4 around the period boundaries") {
    // N starts growing at 614,400 and grows every 51,200 (upstream heights); with d = 4 these are
    // real heights 2,457,600 + k * 204,800. The cap 4,198,400 maps to real height 16,793,600.
    val boundaries = (0 to 4).map(k => 4 * (614400 + k * 51200)) :+ (4 * 4198400)
    val heights = boundaries.flatMap(b => Seq(b - 5, b - 4, b - 1, b, b + 1, b + 3, b + 4))
    heights.foreach { h =>
      val header = template.copy(height = h)
      withClue(s"height $h: ") {
        d4.calcN(header) shouldBe upstream.calcN(Header.Interpreter60Version, h / 4)
      }
    }

    def n4(h: Int): Int = d4.calcN(template.copy(height = h))

    n4(2457599) shouldBe upstream.NBase
    n4(2457600) shouldBe 70464240
    n4(2457603) shouldBe 70464240
    n4(2662399) shouldBe 70464240
    n4(2662400) shouldBe 73987410
    n4(4 * 4198400 - 1) shouldBe upstream.calcN(Header.Interpreter60Version, 4198399)
    n4(4 * 4198400) shouldBe 2143944600

    // upstream N at the same real heights differs (growth started at 614,400)
    upstream.calcN(template.copy(height = 2457600)) should not be 70464240

    // Autolykos v1 is not affected: N is always NBase
    d4.calcN(template.copy(version = Header.InitialVersion, height = 2457600)) shouldBe upstream.NBase
  }

  property("at d = 4 the hit uses height / 4 as table seed and N, the message commits to the real height") {
    val h = 2457605
    val header = template.copy(height = h)
    val msg = d4.msgByHeader(header)
    msg shouldEqual upstream.msgByHeader(header)
    // same header bytes at another real height with the same height / 4 give another message
    d4.msgByHeader(template.copy(height = h + 1)) should not equal msg

    val expected = upstream.hitForVersion2ForMessage(msg, header.powSolution.n,
      Ints.toByteArray(h / 4), upstream.calcN(header.version, h / 4))
    d4.hitForVersion2(header) shouldBe expected
    d4.powHit(header) shouldBe expected
    d4.hitForVersion2(header) should not be upstream.hitForVersion2(header)
  }

  property("a header mined by the CPU prover at d = 4 validates at d = 4 and fails at d = 1") {
    // 2,457,600: height / 4 differs from height both in N and in the table seed;
    // 1,001: only the table seed differs (N is NBase either way)
    Seq(2457600, 1001).foreach { h =>
      withClue(s"height $h: ") {
        val header = mine(d4, candidateAt(h))
        header.height shouldBe h
        d4.validate(header) shouldBe 'success
        d1.validate(header) shouldBe 'failure
        upstream.validate(header) shouldBe 'failure
      }
    }
  }

  property("a header mined at d = 1 fails at d = 4") {
    val header = mine(d1, candidateAt(1001))
    d1.validate(header) shouldBe 'success
    d4.validate(header) shouldBe 'failure
  }

  property("external work message h is height / 4 at d = 4, and a solution for it validates") {
    val h = 2457605
    val candidate = candidateAt(h)
    val pk = sigma.data.ProveDlog(genPk(sk))

    val wm4 = d4.deriveExternalCandidate(candidate, pk)
    wm4.h shouldBe Some(h / 4)
    wm4.asJson.hcursor.downField("h").as[Int] shouldBe Right(h / 4)

    val wm1 = d1.deriveExternalCandidate(candidate, pk)
    wm1.h shouldBe Some(h)
    // msg commits to the real height, so it is the same at both divisors
    wm4.msg shouldEqual wm1.msg

    // an external miner taking the table seed and N from "h" finds a solution valid at d = 4
    val mined = mine(d4, candidate)
    wm4.msg shouldEqual d4.msgByHeader(mined)
    val hit = upstream.hitForVersion2ForMessage(wm4.msg, mined.powSolution.n,
      Ints.toByteArray(wm4.h.get), upstream.calcN(Header.Interpreter60Version, wm4.h.get))
    hit shouldBe d4.hitForVersion2(mined)
    (hit < wm4.b) shouldBe true

    // Autolykos v1 candidates carry no h
    d4.deriveExternalCandidate(candidate.copy(version = Header.InitialVersion), pk).h shouldBe None
  }

  property("powHeightDivisor is read from config, default 1") {
    def read(s: String): AutolykosPowScheme = ConfigFactory.parseString(s).as[AutolykosPowScheme]("p")

    val noKey = read("p { powType = \"autolykos\", k = 32, n = 26 }")
    noKey.powHeightDivisor shouldBe 1
    noKey.getClass shouldBe classOf[AutolykosPowScheme]

    read("p { powType = \"autolykos\", k = 32, n = 26, powHeightDivisor = 4 }").powHeightDivisor shouldBe 4

    val fake = read("p { powType = \"fake\", k = 32, n = 26, powHeightDivisor = 4 }")
    fake shouldBe a[DefaultFakePowScheme]
    fake.powHeightDivisor shouldBe 4

    an[IllegalArgumentException] should be thrownBy read("p { powType = \"autolykos\", k = 32, n = 26, powHeightDivisor = 0 }")
  }

}
