package com.gu.dynamodecs.attributes

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

import java.util.UUID


class AttributeCodecTest extends AnyFreeSpec with Matchers {
  "AttributeCodec" - {
    "String codec" - {
      "can roundtrip data" in {
        val codec = summon[AttributeCodec[String]]
        val input = "a string"
        val encoded = codec.encode(input)
        val decoded = codec.decode(encoded)
        decoded shouldEqual Right(input)
      }

      "fails helpfully if given data of the wrong type" in {
        val codec = summon[AttributeCodec[String]]
        val decoded = codec.decode(AttributeValue.fromN("1234"))
        // TODO: better assertion for a *useful* failure
        decoded.isLeft shouldEqual true
      }
    }

    "Int codec" - {
      "can roundtrip data" in {
        val codec = summon[AttributeCodec[Int]]
        val input = 1234
        val encoded = codec.encode(input)
        val decoded = codec.decode(encoded)
        decoded shouldEqual Right(input)
      }

      "fails helpfully if given data of the wrong type" in {
        val codec = summon[AttributeCodec[Int]]
        val decoded = codec.decode(AttributeValue.fromS("incorrect data"))
        // TODO: better assertion for a *useful* failure
        decoded.isLeft shouldEqual true
      }
    }

    "Long codec" - {
      "can roundtrip data" in {
        val codec = summon[AttributeCodec[Long]]
        val input = 1234L
        val encoded = codec.encode(input)
        val decoded = codec.decode(encoded)
        decoded shouldEqual Right(input)
      }

      "fails helpfully if given data of the wrong type" in {
        val codec = summon[AttributeCodec[Long]]
        val decoded = codec.decode(AttributeValue.fromS("incorrect data"))
        // TODO: better assertion for a *useful* failure
        decoded.isLeft shouldEqual true
      }
    }

    "Double codec" - {
      "can roundtrip data" in {
        val codec = summon[AttributeCodec[Double]]
        val input = 2134.5D
        val encoded = codec.encode(input)
        val decoded = codec.decode(encoded)
        decoded shouldEqual Right(input)
      }

      "fails helpfully if given data of the wrong type" in {
        val codec = summon[AttributeCodec[Double]]
        val decoded = codec.decode(AttributeValue.fromS("incorrect data"))
        // TODO: better assertion for a *useful* failure
        decoded.isLeft shouldEqual true
      }
    }

    "Boolean codec" - {
      "can roundtrip data" in {
        val codec = summon[AttributeCodec[Boolean]]
        val input = false
        val encoded = codec.encode(input)
        val decoded = codec.decode(encoded)
        decoded shouldEqual Right(input)
      }

      "fails helpfully if given data of the wrong type" in {
        val codec = summon[AttributeCodec[Boolean]]
        val decoded = codec.decode(AttributeValue.fromS("incorrect data"))
        // TODO: better assertion for a *useful* failure
        decoded.isLeft shouldEqual true
      }
    }

    "BigDecimal codec" - {
      "can roundtrip data" in {
        val codec = summon[AttributeCodec[BigDecimal]]
        val input = BigDecimal("123456789101112131415161718192021222324252627282930.0123456789")
        val encoded = codec.encode(input)
        val decoded = codec.decode(encoded)
        decoded shouldEqual Right(input)
      }

      "fails helpfully if given data of the wrong type" in {
        val codec = summon[AttributeCodec[BigDecimal]]
        val decoded = codec.decode(AttributeValue.fromS("incorrect data"))
        // TODO: better assertion for a *useful* failure
        decoded.isLeft shouldEqual true
      }
    }

    "BigInt codec" - {
      "can roundtrip data" in {
        val codec = summon[AttributeCodec[BigInt]]
        val input = BigInt("123456789101112131415161718192021222324252627282930")
        val encoded = codec.encode(input)
        val decoded = codec.decode(encoded)
        decoded shouldEqual Right(input)
      }

      "fails helpfully if given data of the wrong type" in {
        val codec = summon[AttributeCodec[BigInt]]
        val decoded = codec.decode(AttributeValue.fromS("incorrect data"))
        // TODO: better assertion for a *useful* failure
        decoded.isLeft shouldEqual true
      }
    }

    "Short codec" - {
      "can roundtrip data" in {
        val codec = summon[AttributeCodec[Short]]
        val input = 2.toShort
        val encoded = codec.encode(input)
        val decoded = codec.decode(encoded)
        decoded shouldEqual Right(input)
      }

      "fails helpfully if given data of the wrong type" in {
        val codec = summon[AttributeCodec[Short]]
        val decoded = codec.decode(AttributeValue.fromS("incorrect data"))
        // TODO: better assertion for a *useful* failure
        decoded.isLeft shouldEqual true
      }
    }

    "UUID codec" - {
      "can roundtrip data" in {
        val codec = summon[AttributeCodec[UUID]]
        val input = UUID.randomUUID()
        val encoded = codec.encode(input)
        val decoded = codec.decode(encoded)
        decoded shouldEqual Right(input)
      }

      "fails helpfully if given a non-UUID string" in {
        val codec = summon[AttributeCodec[UUID]]
        val decoded = codec.decode(AttributeValue.fromS("not a UUID"))
        // TODO: better assertion for a *useful* failure
        decoded.isLeft shouldEqual true
      }

      "fails helpfully if given data of the wrong type" in {
        val codec = summon[AttributeCodec[UUID]]
        val decoded = codec.decode(AttributeValue.fromN("1234"))
        // TODO: better assertion for a *useful* failure
        decoded.isLeft shouldEqual true
      }
    }

    // collections

    "Option codec" - {
      "can round trip a Some" in {
        val codec = summon[AttributeCodec[Option[Boolean]]]
        val encoded = codec.encode(Some(true))
        val decoded = codec.decode(encoded)
        decoded shouldEqual Right(Some(true))
      }

      "can round trip a None" in {
        val codec = summon[AttributeCodec[Option[Boolean]]]
        val encoded = codec.encode(None)
        val decoded = codec.decode(encoded)
        decoded shouldEqual Right(None)
      }

      "fails helpfully if given non-boolean data" in {
        val codec = summon[AttributeCodec[Option[Int]]]
        val decoded = codec.decode(AttributeValue.fromS("hello"))
        // TODO: give more detail here
        decoded.isLeft shouldEqual true
      }
    }

    "int List codec" - {
      "can roundtrip data" in {
        val codec = summon[AttributeCodec[List[Int]]]
        val input = List(1, 2, 3)
        val encoded = codec.encode(input)
        val decoded = codec.decode(encoded)
        decoded shouldEqual Right(input)
      }

      "fails if given data of the wrong type" - {
        "given a string when expecting a list of ints" in {
          val data = AttributeValue.builder().s("1").build()
          val codec = summon[AttributeCodec[List[Int]]]
          val decoded = codec.decode(data)
          decoded.isLeft shouldEqual true
        }

        "given a boolean when expecting a list of ints" in {
          val data = AttributeValue.builder().bool(true).build()
          val codec = summon[AttributeCodec[List[Int]]]
          val decoded = codec.decode(data)
          decoded.isLeft shouldEqual true
        }

        "given a single int when expecting a list of ints" in {
          val data = AttributeValue.builder().n("1").build()
          val codec = summon[AttributeCodec[List[Int]]]
          val decoded = codec.decode(data)
          decoded.isLeft shouldEqual true
        }
      }
    }

    "generic List codec" - {
      "can roundtrip data" in {
        val codec = summon[AttributeCodec[List[Int]]]
        val input = List(1, 2, 3)
        val encoded = codec.encode(input)
        val decoded = codec.decode(encoded)
        decoded shouldEqual Right(input)
      }

      "fails if given data of the wrong type" - {
        "given a single int when expecting a list of ints" in {
          val data = AttributeValue.builder().n("1").build()
          val codec = summon[AttributeCodec[List[Int]]]
          val decoded = codec.decode(data)
          decoded.isLeft shouldEqual true
        }

        "given a string when expecting a list of ints" in {
          val data = AttributeValue.builder().s("1").build()
          val codec = summon[AttributeCodec[List[Int]]]
          val decoded = codec.decode(data)
          decoded.isLeft shouldEqual true
        }
      }
    }
  }
}
