package com.gu.dynamodecs.attributes

import com.gu.dynamodecs.{DynamodecDecodeError, DynamodecResult}
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

import java.util.UUID
import scala.compiletime.{erasedValue, summonInline}
import scala.jdk.CollectionConverters.*
import scala.util.Try
import com.gu.dynamodecs.Utils.*


trait AttributeCodec[A]:
  def encode(a: A): AttributeValue
  def decode(av: AttributeValue): DynamodecResult[A]

object AttributeCodec:
  def apply[A](using AttributeCodec[A]): AttributeCodec[A] =
    summon[AttributeCodec[A]]

  // helper methods

  extension [A](avc: AttributeCodec[A])
    def imap[B](f: A => B)(g: B => A): AttributeCodec[B] =
      new AttributeCodec[B]:
        def encode(b: B): AttributeValue = avc.encode(g(b))
        def decode(av: AttributeValue): DynamodecResult[B] = avc.decode(av).map(f)

    def iemap[B](f: A => DynamodecResult[B])(g: B => A): AttributeCodec[B] =
      new AttributeCodec[B]:
        def encode(b: B): AttributeValue = avc.encode(g(b))
        def decode(av: AttributeValue): DynamodecResult[B] = avc.decode(av).flatMap(f)

  // create an instance

  def instance[A](encodeValue: A => AttributeValue)(decodeValue: AttributeValue => A): AttributeCodec[A] =
    new AttributeCodec[A]:
      def encode(a: A): AttributeValue = encodeValue(a)
      def decode(av: AttributeValue): DynamodecResult[A] = Right(decodeValue(av))

  // default instances

  given AttributeCodec[String] with
    def encode(s: String): AttributeValue = AttributeValue.fromS(s)
    def decode(av: AttributeValue): DynamodecResult[String] = Right(av.s())

  given AttributeCodec[Int] with
    def encode(n: Int): AttributeValue = AttributeValue.fromN(n.toString)
    def decode(av: AttributeValue): DynamodecResult[Int] =
      Try(av.n().toInt)
        .toEither
        .left.map(e => DynamodecDecodeError(s"Failed to decode Int from AttributeValue", e))

  given AttributeCodec[Long] with
    def encode(l: Long): AttributeValue = AttributeValue.fromN(l.toString)
    def decode(av: AttributeValue): DynamodecResult[Long] =
      Try(av.n().toLong)
        .toEither
        .left.map(e => DynamodecDecodeError(s"Failed to decode Long from AttributeValue", e))

  given AttributeCodec[Double] with
    def encode(d: Double): AttributeValue = AttributeValue.fromN(d.toString)
    def decode(av: AttributeValue): DynamodecResult[Double] =
      Try(av.n().toDouble)
        .toEither
        .left.map(e => DynamodecDecodeError(s"Failed to decode Double from AttributeValue", e))

  given AttributeCodec[Boolean] with
    def encode(b: Boolean): AttributeValue = AttributeValue.fromBool(b)
    def decode(av: AttributeValue): DynamodecResult[Boolean] =
      // TODO: this currently returns Right(false) if the type of data in the av is wrong
      // how can we detect incorrectly typed data?

      Right(av.bool())

  given AttributeCodec[BigDecimal] with
    def encode(bd: BigDecimal): AttributeValue = AttributeValue.fromN(bd.toString)
    def decode(av: AttributeValue): DynamodecResult[BigDecimal] =
      // TODO: how does this fail?
      Try(BigDecimal(av.n()))
        .toEither
        .left.map(e => DynamodecDecodeError(s"Failed to decode BigDecimal from AttributeValue", e))

  given AttributeCodec[BigInt] with
    def encode(bi: BigInt): AttributeValue = AttributeValue.fromN(bi.toString)
    def decode(av: AttributeValue): DynamodecResult[BigInt] =
      // TODO: how does this fail?
      Try(BigInt(av.n()))
        .toEither
        .left.map(e => DynamodecDecodeError(s"Failed to decode BigInt from AttributeValue", e))

  given AttributeCodec[Short] with
    def encode(s: Short): AttributeValue = AttributeValue.fromN(s.toString)
    def decode(av: AttributeValue): DynamodecResult[Short] =
      // TODO: how does this fail?
      Try(av.n().toShort)
        .toEither
        .left.map(e => DynamodecDecodeError(s"Failed to decode Short from AttributeValue", e))

  given AttributeCodec[UUID] with
    def encode(uuid: UUID): AttributeValue = AttributeValue.fromS(uuid.toString)
    def decode(av: AttributeValue): DynamodecResult[UUID] =
      Option(av.s())
        .toRight(DynamodecDecodeError(s"Failed to decode UUID from AttributeValue"))
        .flatMap: s =>
          Try(UUID.fromString(s))
            .toEither
            .left.map(e => DynamodecDecodeError(s"Failed to decode UUID from AttributeValue", e))

  given [A](using codec: AttributeCodec[A]): AttributeCodec[Option[A]] with
    def encode(a: Option[A]): AttributeValue = a.map(codec.encode).getOrElse(AttributeValue.fromNul(true))
    def decode(av: AttributeValue): DynamodecResult[Option[A]] =
      if av.nul() then Right(None) else codec.decode(av).map(Some(_))

  given [A](using codec: AttributeCodec[A]): AttributeCodec[List[A]] with
    def encode(a: List[A]): AttributeValue = AttributeValue.fromL(a.map(codec.encode).asJava)
    def decode(av: AttributeValue): DynamodecResult[List[A]] =
      // TODO: fix implicits so that this can be traversed
      av.l().asScala.toList.traverseE(codec.decode)

  given [A](using codec: AttributeCodec[A]): AttributeCodec[Seq[A]] with
    def encode(a: Seq[A]): AttributeValue =
      AttributeValue.fromL(a.map(codec.encode).asJava)
    def decode(av: AttributeValue): DynamodecResult[Seq[A]] =
      av.l().asScala.toSeq.traverseE(codec.decode)

  given [A](using codec: AttributeCodec[A]): AttributeCodec[Set[A]] with
    def encode(a: Set[A]): AttributeValue =
      AttributeValue.builder().l(a.map(codec.encode).asJava).build()
    def decode(av: AttributeValue): DynamodecResult[Set[A]] =
      av.l().asScala.toList.traverseE(codec.decode).map(_.toSet)

  given [V](using valueCodec: AttributeCodec[V]): AttributeCodec[Map[String, V]] =
    new AttributeCodec[Map[String, V]]:
      override def encode(a: Map[String, V]): AttributeValue =
        val entries =
          a.map { case (k, v) => k -> valueCodec.encode(v) }
        AttributeValue.fromM(entries.asJava)

      override def decode(av: AttributeValue): DynamodecResult[Map[String, V]] =
        av.m().asScala.toMap.traverseE(valueCodec.decode)

  given [K, V](using keyCodec: MapKeyCodec[K], valueCodec: AttributeCodec[V]): AttributeCodec[Map[K, V]] =
    new AttributeCodec[Map[K, V]]:
      override def encode(a: Map[K, V]): AttributeValue =
        val entries =
          a.map { case (k, v) => keyCodec.encodeKey(k) -> valueCodec.encode(v) }
        AttributeValue.fromM(entries.asJava)
      override def decode(av: AttributeValue): DynamodecResult[Map[K, V]] =
        av.m().asScala.toMap.traverseEKV { case (k, v) =>
          keyCodec.decodeKey(k).flatMap { k =>
            // TODO: add field name to the nested error message
            valueCodec.decode(v).map(k -> _)
          }
        }

  // instance derivation

  inline given derived[A : io.circe.Codec]: AttributeCodec[A] =
    val jsonCodec = summon[io.circe.Codec[A]]

    new AttributeCodec[A]:
      override def encode(a: A): AttributeValue =
        val json = jsonCodec(a)
        AttributeCodec[String].encode(json.noSpaces)

      override def decode(av: AttributeValue): DynamodecResult[A] =
        val result = for
          rawJson <- AttributeCodec[String].decode(av)
          json <- io.circe.parser.parse(rawJson)
            .left.map(e => DynamodecDecodeError(s"Failed to parse JSON from AttributeValue", e))
          a <- json.as[A](jsonCodec)
            .left.map(e => DynamodecDecodeError(s"Failed to decode JSON from AttributeValue", e))
        yield a
        // TODO: refactor error handling into the codec interface
        result
