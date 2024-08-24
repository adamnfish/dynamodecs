package com.adamnfish.dynamodecs.items

import com.adamnfish.dynamodecs.attributes.{MapKeyCodec, UnwrappedAttributeCodec}
import io.circe.*
import io.circe.generic.semiauto.*
import org.scalatest.EitherValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import software.amazon.awssdk.services.dynamodb.model.AttributeValue


class ItemCodecTest extends AnyFreeSpec with Matchers with EitherValues {
  "round-trips" - {
    "a simple codec" - {
      "can round trip a valid datastructure" in {
        case class Foo(a: String, b: Boolean, c: Int) derives ItemCodec

        val foo = Foo("hello", true, 42)
        val encodedFoo = summon[ItemCodec[Foo]].encode(foo)
        val decodedFoo = summon[ItemCodec[Foo]].decode(encodedFoo).value

        decodedFoo shouldEqual foo
      }
      
      "fails to deocde data that does not match the expected type" in {
        case class Foo(a: String, b: Boolean, c: Int) derives ItemCodec

        val encodedFoo = Map(
          "a" -> AttributeValue.builder().s("hello").build(),
          "b" -> AttributeValue.builder().s("not a boolean").build(),
          "c" -> AttributeValue.builder().s("not an int").build()
        )
        summon[ItemCodec[Foo]].decode(encodedFoo).isLeft shouldBe true
      }
    }

    "a codec with a collection" - {
      "can round trip a valid datastructure" in {
        case class Foo(a: String, ns: List[Int]) derives ItemCodec

        val foo = Foo("hello", List(42, 7))
        val encodedFoo = summon[ItemCodec[Foo]].encode(foo)
        val decodedFoo = summon[ItemCodec[Foo]].decode(encodedFoo).value

        decodedFoo shouldEqual foo
      }
    }

    "a datastructure with an optional value" - {
      "works when it is present" in {
        case class Foo(a: String, maybeInt: Option[Int]) derives ItemCodec

        val foo = Foo("hello", Some(42))
        val encodedFoo = summon[ItemCodec[Foo]].encode(foo)
        val decodedFoo = summon[ItemCodec[Foo]].decode(encodedFoo).value

        decodedFoo shouldEqual foo
      }

      "works when it is absent" in {
        case class Foo(a: String, maybeInt: Option[Int]) derives ItemCodec

        val foo = Foo("hello", None)
        val encodedFoo = summon[ItemCodec[Foo]].encode(foo)
        val decodedFoo = summon[ItemCodec[Foo]].decode(encodedFoo).value

        decodedFoo shouldEqual foo
      }

      "cannot decode an optional value containing the wrong type" in {
        case class Foo(str: String, maybeInt: Option[Int]) derives ItemCodec

        val encodedFoo = Map(
          "str" -> AttributeValue.builder().s("hello").build(),
          "maybeInt" -> AttributeValue.builder().s("not an int").build()
        )
        summon[ItemCodec[Foo]].decode(encodedFoo).isLeft shouldBe true
      }
    }

    "a datastructure with a Map" - {
      "works with String keys" in {
        case class Foo(a: String, map: Map[String, Int]) derives ItemCodec

        val foo = Foo("hello", Map("a" -> 1, "b" -> 2))
        val encodedFoo = summon[ItemCodec[Foo]].encode(foo)
        val decodedFoo = summon[ItemCodec[Foo]].decode(encodedFoo).value

        decodedFoo shouldEqual foo
      }

      "works with non-string keys that have a provided MapKeyCodec instance" in {
        case class Foo(a: String, map: Map[Int, Int]) derives ItemCodec

        val foo = Foo("hello", Map(1 -> 1, 2 -> 2))
        val encodedFoo = summon[ItemCodec[Foo]].encode(foo)
        val decodedFoo = summon[ItemCodec[Foo]].decode(encodedFoo).value

        decodedFoo shouldEqual foo
      }

      "works with non-primitive where a MapKeyCodec instance is provided" in {
        case class KeyType(key: String)
        given MapKeyCodec[KeyType] = MapKeyCodec.instance(KeyType.apply)(_.key)
        case class Foo(a: String, map: Map[KeyType, Int]) derives ItemCodec

        val foo = Foo("hello", Map(KeyType("test") -> 1, KeyType("test2") -> 2))
        val encodedFoo = summon[ItemCodec[Foo]].encode(foo)
        val decodedFoo = summon[ItemCodec[Foo]].decode(encodedFoo)

        decodedFoo.value shouldEqual foo
      }

      "does not work with non-string keys that do not have a MapKeyCodec instance" in {
        case class KeyType(key: String)
        case class Foo(a: String, map: Map[KeyType, Int])

        "summon[ItemCodec[Foo]]" shouldNot compile
      }
    }

    "JSON serialises a case class field" in {
      case class Foo(a: Int, bar: Bar) derives ItemCodec
      case class Bar(b: String)
      given Codec[Bar] = deriveCodec

      val foo = Foo(42, Bar("hello"))
      val encodedFoo = summon[ItemCodec[Foo]].encode(foo)
      val decodedFoo = summon[ItemCodec[Foo]].decode(encodedFoo).value

      decodedFoo shouldEqual foo
    }

    "JSON serialises a multiply nested case class field" in {
      case class Foo(a: Int, bar: Bar) derives ItemCodec
      case class Bar(b: String, baz: Baz)
      case class Baz(c: Boolean)
      given Codec[Baz] = deriveCodec
      given Codec[Bar] = deriveCodec

      val foo = Foo(42, Bar("hello", Baz(true)))
      val encodedFoo = summon[ItemCodec[Foo]].encode(foo)
      val decodedFoo = summon[ItemCodec[Foo]].decode(encodedFoo).value

      decodedFoo shouldEqual foo
    }

    "a datastructure containing wrapped fields" in {
      case class Foo(a: Int, bar: Bar) derives ItemCodec
      case class Bar(b: String)
      given UnwrappedAttributeCodec[Bar] = UnwrappedAttributeCodec.deriveUnwrapper

      val foo = Foo(42, Bar("hello"))
      val encodedFoo = summon[ItemCodec[Foo]].encode(foo)
      val decodedFoo = summon[ItemCodec[Foo]].decode(encodedFoo).value

      decodedFoo shouldEqual foo
    }
  }

  "value class fields" - {
    "are unwrapped by providing an UnwrappedAttributeCodec instance" in {
      case class Foo(a: Int, bar: Bar) derives ItemCodec
      case class Bar(b: String)
      given UnwrappedAttributeCodec[Bar] = UnwrappedAttributeCodec.deriveUnwrapper

      val foo = Foo(42, Bar("hello"))
      val encodedFoo = summon[ItemCodec[Foo]].encode(foo)

      encodedFoo.get("bar") shouldEqual Some(AttributeValue.fromS("hello"))
    }

    "are normally wrapped with a provided JSON codec" in {
      case class Foo(a: Int, bar: Bar) derives ItemCodec
      case class Bar(b: String)
      given Codec[Bar] = deriveCodec

      val foo = Foo(42, Bar("hello"))
      val encodedFoo = summon[ItemCodec[Foo]].encode(foo)

      encodedFoo.get("bar") shouldEqual Some(AttributeValue.fromS("""{"b":"hello"}"""))
    }

    "are unwrapped if both a JSON Codec and Unwrapper instance are present" in {
      case class Foo(a: Int, bar: Bar) derives ItemCodec
      case class Bar(b: String)
      given Codec[Bar] = deriveCodec
      given UnwrappedAttributeCodec[Bar] = UnwrappedAttributeCodec.deriveUnwrapper

      val foo = Foo(42, Bar("hello"))
      val encodedFoo = summon[ItemCodec[Foo]].encode(foo)

      encodedFoo.get("bar") shouldEqual Some(AttributeValue.fromS("hello"))
    }
  }

  "Can manually summon a codec" in {
    case class Bar(a: Int)
    val bar = Bar(42)
    val barItemCodec = summon[ItemCodec[Bar]]
    barItemCodec.decode(barItemCodec.encode(bar)) shouldEqual Right(bar)
  }

  "Can manually create a codec" ignore {
    // TODO: Once error handling is in place to save having to rewrite this
  }

  "deriving an ItemCodec does not compile if there is no JSON codec available for the nested type" in {
    case class Foo(a: Int, bar: Bar)
    case class Bar(b: String)

    "summon[ItemCodec[Foo]]" shouldNot compile
  }

  "syntax" - {
    "asDbItem works the same way as manually encoding" in {
      import ItemCodec.asDbItem
      case class Foo(s: String, n: Int)
      val foo = Foo("hello", 42)
      foo.asDbItem shouldEqual summon[ItemCodec[Foo]].encode(foo)
    }

    "fromDbItem works the same way as manually decoding" in {
      import ItemCodec.fromDbItem
      case class Foo(s: String, n: Int)
      val foo = Foo("hello", 42)
      val encodedItem = summon[ItemCodec[Foo]].encode(foo)
      encodedItem.fromDbItem[Foo] shouldEqual summon[ItemCodec[Foo]].decode(encodedItem)
    }
  }
}
