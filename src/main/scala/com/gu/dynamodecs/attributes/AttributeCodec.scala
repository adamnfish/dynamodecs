package com.gu.dynamodecs.attributes

import com.gu.dynamodecs.{DynamodecDecodeError, DynamodecResult}
import software.amazon.awssdk.services.dynamodb.model.AttributeValue
import cats.*
import cats.data.*
import cats.syntax.all.*

import java.util.UUID
import scala.compiletime.{erasedValue, summonInline}
import scala.jdk.CollectionConverters.*
import scala.util.Try


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

  // create an instance

  def instance[A](encodeValue: A => AttributeValue)(decodeValue: AttributeValue => A): AttributeCodec[A] =
    new AttributeCodec[A]:
      def encode(a: A): AttributeValue = encodeValue(a)
      def decode(av: AttributeValue): DynamodecResult[A] = Right(decodeValue(av))

  // default instances

  given AttributeCodec[String] with
    def encode(a: String): AttributeValue = AttributeValue.builder().s(a).build()
    def decode(av: AttributeValue): DynamodecResult[String] = Right(av.s())

  given AttributeCodec[Int] with
    def encode(a: Int): AttributeValue = AttributeValue.builder().n(a.toString).build()
    def decode(av: AttributeValue): DynamodecResult[Int] =
      Try(av.n().toInt)
        .toEither
        .left.map(e => DynamodecDecodeError(s"Failed to decode Int from AttributeValue", e))

  given AttributeCodec[Long] with
    def encode(a: Long): AttributeValue = AttributeValue.builder().n(a.toString).build()
    def decode(av: AttributeValue): DynamodecResult[Long] =
      Try(av.n().toLong)
        .toEither
        .left.map(e => DynamodecDecodeError(s"Failed to decode Long from AttributeValue", e))

  given AttributeCodec[Double] with
    def encode(a: Double): AttributeValue = AttributeValue.builder().n(a.toString).build()
    def decode(av: AttributeValue): DynamodecResult[Double] =
      Try(av.n().toDouble)
        .toEither
        .left.map(e => DynamodecDecodeError(s"Failed to decode Double from AttributeValue", e))

  given AttributeCodec[Boolean] with
    def encode(a: Boolean): AttributeValue = AttributeValue.builder().bool(a).build()
    def decode(av: AttributeValue): DynamodecResult[Boolean] = Right(av.bool()) // TODO: how does this fail?

  given AttributeCodec[BigDecimal] with
    def encode(a: BigDecimal): AttributeValue = AttributeValue.builder().n(a.toString).build()
    def decode(av: AttributeValue): DynamodecResult[BigDecimal] =
      // TODO: how does this fail?
      Try(BigDecimal(av.n()))
        .toEither
        .left.map(e => DynamodecDecodeError(s"Failed to decode BigDecimal from AttributeValue", e))

  given AttributeCodec[BigInt] with
    def encode(a: BigInt): AttributeValue = AttributeValue.builder().n(a.toString).build()
    def decode(av: AttributeValue): DynamodecResult[BigInt] =
      // TODO: how does this fail?
      Try(BigInt(av.n()))
        .toEither
        .left.map(e => DynamodecDecodeError(s"Failed to decode BigInt from AttributeValue", e))

  given AttributeCodec[Short] with
    def encode(a: Short): AttributeValue = AttributeValue.builder().n(a.toString).build()
    def decode(av: AttributeValue): DynamodecResult[Short] =
      // TODO: how does this fail?
      Try(av.n().toShort)
        .toEither
        .left.map(e => DynamodecDecodeError(s"Failed to decode Short from AttributeValue", e))

  given AttributeCodec[UUID] with
    def encode(a: UUID): AttributeValue = AttributeValue.builder().s(a.toString).build()
    def decode(av: AttributeValue): DynamodecResult[UUID] =
      // TODO: how does this fail?
      Right(UUID.fromString(av.s()))

  given [A](using codec: AttributeCodec[A]): AttributeCodec[Option[A]] with
    def encode(a: Option[A]): AttributeValue = a.map(codec.encode).getOrElse(AttributeValue.builder().nul(true).build())
    def decode(av: AttributeValue): DynamodecResult[Option[A]] =
      if av.nul() then Right(None) else codec.decode(av).map(Some(_))

  given [A](using codec: AttributeCodec[A]): AttributeCodec[List[A]] with
    def encode(a: List[A]): AttributeValue = AttributeValue.builder().l(a.map(codec.encode).asJava).build()
    def decode(av: AttributeValue): DynamodecResult[List[A]] =
      // TODO: fix implicits so that this can be traversed
      av.l().asScala.toList.traverse(codec.decode)

  given [A](using codec: AttributeCodec[A]): AttributeCodec[Seq[A]] with
    def encode(a: Seq[A]): AttributeValue =
      AttributeValue.builder().l(a.map(codec.encode).asJava).build()
    def decode(av: AttributeValue): DynamodecResult[Seq[A]] =
      av.l().asScala.toSeq.traverse(codec.decode)

  given [A](using codec: AttributeCodec[A]): AttributeCodec[Set[A]] with
    def encode(a: Set[A]): AttributeValue =
      AttributeValue.builder().l(a.map(codec.encode).asJava).build()
    def decode(av: AttributeValue): DynamodecResult[Set[A]] =
      av.l().asScala.toList.traverse(codec.decode).map(_.toSet)

  given [V](using valueCodec: AttributeCodec[V]): AttributeCodec[Map[String, V]] =
    new AttributeCodec[Map[String, V]]:
      override def encode(a: Map[String, V]): AttributeValue =
        val entries =
          a.map { case (k, v) => k -> valueCodec.encode(v) }
        AttributeValue.builder().m(entries.asJava).build()

      override def decode(av: AttributeValue): DynamodecResult[Map[String, V]] =
        av.m().asScala.toList.traverse { case (k, v) =>
          valueCodec.decode(v).map(k -> _)
        }.map(_.toMap)

  given [K, V](using keyCodec: MapKeyCodec[K], valueCodec: AttributeCodec[V]): AttributeCodec[Map[K, V]] =
    new AttributeCodec[Map[K, V]]:
      override def encode(a: Map[K, V]): AttributeValue =
        val entries =
          a.map { case (k, v) => keyCodec.encodeKey(k) -> valueCodec.encode(v) }
        AttributeValue.builder().m(entries.asJava).build()

      override def decode(av: AttributeValue): DynamodecResult[Map[K, V]] =
        av.m().asScala.toList.traverse { case (k, v) =>
          for
            key <- keyCodec.decodeKey(k)
            value <- valueCodec.decode(v)
           yield (key, value)
        }.map(_.toMap)

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
