package com.adamnfish.dynamodecs.attributes

import com.adamnfish.dynamodecs.{DynamodecDecodeError, DynamodecResult}
import com.adamnfish.dynamodecs.Utils.*
import io.circe
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

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

  def encode[A](a: A)(using codec: AttributeCodec[A]): AttributeValue =
    codec.encode(a)

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
    def decode(av: AttributeValue): DynamodecResult[String] =
      if av.`type`() == AttributeValue.Type.S then Right(av.s())
      else Left(DynamodecDecodeError(s"Failed to decode String from AttributeValue"))

  given AttributeCodec[Int] with
    def encode(n: Int): AttributeValue = AttributeValue.fromN(n.toString)
    def decode(av: AttributeValue): DynamodecResult[Int] =
      if av.`type`() != AttributeValue.Type.N then
        Left(DynamodecDecodeError(s"Failed to decode Int from AttributeValue"))
      else
        Try(av.n().toInt)
          .toEither
          .left.map(e => DynamodecDecodeError(s"Failed to decode Int from AttributeValue", e))

  given AttributeCodec[Long] with
    def encode(l: Long): AttributeValue = AttributeValue.fromN(l.toString)
    def decode(av: AttributeValue): DynamodecResult[Long] =
      if av.`type`() != AttributeValue.Type.N then
        Left(DynamodecDecodeError(s"Failed to decode Long from AttributeValue"))
      else
        Try(av.n().toLong)
          .toEither
          .left.map(e => DynamodecDecodeError(s"Failed to decode Long from AttributeValue", e))

  given AttributeCodec[Double] with
    def encode(d: Double): AttributeValue = AttributeValue.fromN(d.toString)
    def decode(av: AttributeValue): DynamodecResult[Double] =
      if av.`type`() != AttributeValue.Type.N then
        Left(DynamodecDecodeError(s"Failed to decode Double from AttributeValue"))
      else
        Try(av.n().toDouble)
          .toEither
          .left.map(e => DynamodecDecodeError(s"Failed to decode Double from AttributeValue", e))

  given AttributeCodec[Short] with
    def encode(s: Short): AttributeValue = AttributeValue.fromN(s.toString)
    def decode(av: AttributeValue): DynamodecResult[Short] =
      if av.`type`() != AttributeValue.Type.N then
        Left(DynamodecDecodeError(s"Failed to decode Short from AttributeValue"))
      else
        Try(av.n().toShort)
          .toEither
          .left.map(e => DynamodecDecodeError(s"Failed to decode Short from AttributeValue", e))

  given AttributeCodec[BigDecimal] with
    def encode(bd: BigDecimal): AttributeValue = AttributeValue.fromS(bd.toString)
    def decode(av: AttributeValue): DynamodecResult[BigDecimal] =
      Try(BigDecimal(av.s()))
        .toEither
        .left.map(e => DynamodecDecodeError(s"Failed to decode BigDecimal from AttributeValue", e))

  given AttributeCodec[BigInt] with
    def encode(bi: BigInt): AttributeValue = AttributeValue.fromS(bi.toString)
    def decode(av: AttributeValue): DynamodecResult[BigInt] =
      Try(BigInt(av.s()))
        .toEither
        .left.map(e => DynamodecDecodeError(s"Failed to decode BigInt from AttributeValue", e))

  given AttributeCodec[Boolean] with
    def encode(b: Boolean): AttributeValue = AttributeValue.fromBool(b)
    def decode(av: AttributeValue): DynamodecResult[Boolean] =
      if av.`type`() != AttributeValue.Type.BOOL then
        Left(DynamodecDecodeError(s"Failed to decode Boolean from AttributeValue"))
      else
        Right(av.bool())

  /**
   * Encodes a UUID as a String.
   */
  given AttributeCodec[UUID] = AttributeCodec[String].iemap { s =>
    Try(UUID.fromString(s))
      .toEither
      .left.map(e => DynamodecDecodeError(s"Failed to decode UUID from AttributeValue", e))
  }(_.toString)

  given [A](using codec: AttributeCodec[A]): AttributeCodec[Option[A]] with
    def encode(a: Option[A]): AttributeValue = a.map(codec.encode).getOrElse(AttributeValue.fromNul(true))
    def decode(av: AttributeValue): DynamodecResult[Option[A]] =
      if av.nul() then Right(None) else codec.decode(av).map(Some(_))

  given [A](using codec: AttributeCodec[A]): AttributeCodec[List[A]] with
    def encode(a: List[A]): AttributeValue = AttributeValue.fromL(a.map(codec.encode).asJava)
    def decode(av: AttributeValue): DynamodecResult[List[A]] =
      if av.`type`() != AttributeValue.Type.L then
        Left(DynamodecDecodeError(s"Failed to decode List from AttributeValue"))
      else
        av.l().asScala.toList.traverseE(codec.decode)

  given [A](using codec: AttributeCodec[A]): AttributeCodec[Seq[A]] with
    def encode(a: Seq[A]): AttributeValue =
      AttributeValue.fromL(a.map(codec.encode).asJava)
    def decode(av: AttributeValue): DynamodecResult[Seq[A]] =
      if av.`type`() != AttributeValue.Type.L then
        Left(DynamodecDecodeError(s"Failed to decode Seq from AttributeValue"))
      else
        av.l().asScala.toSeq.traverseE(codec.decode)

  given [A](using codec: AttributeCodec[A]): AttributeCodec[Vector[A]] with
    def encode(a: Vector[A]): AttributeValue =
      AttributeValue.fromL(a.map(codec.encode).asJava)
    def decode(av: AttributeValue): DynamodecResult[Vector[A]] =
      if av.`type`() != AttributeValue.Type.L then
        Left(DynamodecDecodeError(s"Failed to decode Vector from AttributeValue"))
      else
        av.l().asScala.toVector.traverseE(codec.decode)

  /**
   * A special case for encoding Sets of Strings using DynamoDB's native support for String Sets (SS).
   */
  given strSetAC: AttributeCodec[Set[String]] with
    def encode(ss: Set[String]): AttributeValue =
      AttributeValue.builder().ss(ss.asJava).build()
    def decode(av: AttributeValue): DynamodecResult[Set[String]] =
      if av.`type`() != AttributeValue.Type.SS then
        Left(DynamodecDecodeError(s"Failed to decode String Set from AttributeValue"))
      else
        Right(av.ss().asScala.toSet)

  /**
   * A special case for encoding Sets of Integers using DynamoDB's native support for Number Sets (NS).
   */
  given intSetAC: AttributeCodec[Set[Int]] with
    def encode(ns: Set[Int]): AttributeValue =
      AttributeValue.builder().ns(ns.map(_.toString).toList.asJava).build()
    def decode(av: AttributeValue): DynamodecResult[Set[Int]] =
      if av.`type`() != AttributeValue.Type.NS then
        Left(DynamodecDecodeError(s"Failed to decode Number Set from AttributeValue"))
      else
        av.ns().asScala.toSet.traverseE: s =>
          Try(s.toInt)
            .toEither
            .left.map(e => DynamodecDecodeError(s"Failed to decode Int from NumberSet AttributeValue", e))

  /**
   * A special case for encoding Sets of Longs using DynamoDB's native support for Number Sets (NS).
   */
  given longSetAC: AttributeCodec[Set[Long]] with
    def encode(ls: Set[Long]): AttributeValue =
      AttributeValue.builder().ns(ls.map(_.toString).toList.asJava).build()
    def decode(av: AttributeValue): DynamodecResult[Set[Long]] =
      if av.`type`() != AttributeValue.Type.NS then
        Left(DynamodecDecodeError(s"Failed to decode Number Set from AttributeValue"))
      else
        av.ns().asScala.toSet.traverseE: s =>
          Try(s.toLong)
            .toEither
            .left.map(e => DynamodecDecodeError(s"Failed to decode Long from NumberSet AttributeValue", e))

  /**
   * A special case for encoding Sets of Doubles using DynamoDB's native support for Number Sets (NS).
   */
  given doubleSetAC: AttributeCodec[Set[Double]] with
    def encode(ds: Set[Double]): AttributeValue =
      AttributeValue.builder().ns(ds.map(_.toString).toList.asJava).build()
    def decode(av: AttributeValue): DynamodecResult[Set[Double]] =
      if av.`type`() != AttributeValue.Type.NS then
        Left(DynamodecDecodeError(s"Failed to decode Number Set from AttributeValue"))
      else
        av.ns().asScala.toSet.traverseE: s =>
          Try(s.toDouble)
            .toEither
            .left.map(e => DynamodecDecodeError(s"Failed to decode Double from NumberSet AttributeValue", e))

  /**
   *
   */
  given shortSetAC: AttributeCodec[Set[Short]] with
    def encode(ss: Set[Short]): AttributeValue =
      AttributeValue.builder().ns(ss.map(_.toString).toList.asJava).build()
    def decode(av: AttributeValue): DynamodecResult[Set[Short]] =
      if av.`type`() != AttributeValue.Type.NS then
        Left(DynamodecDecodeError(s"Failed to decode Number Set from AttributeValue"))
      else
        av.ns().asScala.toSet.traverseE: s =>
          Try(s.toShort)
            .toEither
            .left.map(e => DynamodecDecodeError(s"Failed to decode Short from NumberSet AttributeValue", e))

  /**
   * Encodes a Set of arbitrary type A using a List of AttributeValues.
   *
   * Note: DynamoDB only natively supports sets of numbers, strings and binary data.
   * This means the generic implementation needs to use DynamoDB's 'L' (list) type.
   *
   * For Sets types with native support (e.g. String, Int, Long, Double), we provide special-case instances above.
   */
  given genericSetAC[A](using codec: AttributeCodec[A]): AttributeCodec[Set[A]] with
    def encode(as: Set[A]): AttributeValue =
      AttributeValue.builder().l(as.map(codec.encode).asJava).build()
    def decode(av: AttributeValue): DynamodecResult[Set[A]] =
      if av.`type`() != AttributeValue.Type.L then
        Left(DynamodecDecodeError(s"Failed to decode Set from AttributeValue"))
      else
        av.l().asScala.toList.traverseE(codec.decode).map(_.toSet)

  /**
   * Encodes a Map where the keys are strings.
   *
   * We special-case this common use-case to improve ergonomics,
   * since we can use the default AttributeCodec[String] to encode the key.
   */
  given [V](using valueCodec: AttributeCodec[V]): AttributeCodec[Map[String, V]] =
    new AttributeCodec[Map[String, V]]:
      override def encode(a: Map[String, V]): AttributeValue =
        val entries =
          a.map:
            case (k, v) => k -> valueCodec.encode(v)
        AttributeValue.fromM(entries.asJava)

      override def decode(av: AttributeValue): DynamodecResult[Map[String, V]] =
        if av.`type`() != AttributeValue.Type.M then
          Left(DynamodecDecodeError(s"Failed to decode Map from AttributeValue"))
        else
          av.m().asScala.toMap.traverseE(valueCodec.decode)

  /**
   * Encodes a Map with arbitrary Key and Value types.
   *
   * To do this we need a MapKeyCodec for the keys, and an AttributeCodec for the values.
   */
  given [K, V](using keyCodec: MapKeyCodec[K], valueCodec: AttributeCodec[V]): AttributeCodec[Map[K, V]] =
    new AttributeCodec[Map[K, V]]:
      override def encode(a: Map[K, V]): AttributeValue =
        val entries =
          a.map:
            case (k, v) => keyCodec.encodeKey(k) -> valueCodec.encode(v)
        AttributeValue.fromM(entries.asJava)
      override def decode(av: AttributeValue): DynamodecResult[Map[K, V]] =
        if av.`type`() != AttributeValue.Type.M then
          Left(DynamodecDecodeError(s"Failed to decode Map from AttributeValue"))
        else
          av.m().asScala.toMap.traverseEKV:
            case (label, rawValue) =>
              for
                k <- keyCodec.decodeKey(label)
                v <- valueCodec.decode(rawValue) match
                  case Left(err) => Left(DynamodecDecodeError(s"Failed to decode Map value for '$label'", err))
                  case Right(v) => Right(v)
              yield (k, v)


  // instance derivation

  inline given derived[A : circe.Codec]: AttributeCodec[A] =
    val jsonCodec = summon[circe.Codec[A]]

    new AttributeCodec[A]:
      override def encode(a: A): AttributeValue =
        val json = jsonCodec(a)
        AttributeCodec[String].encode(json.noSpaces)

      override def decode(av: AttributeValue): DynamodecResult[A] =
        for
          rawJson <- AttributeCodec[String].decode(av)
          json <- circe.parser.parse(rawJson)
            .left.map(e => DynamodecDecodeError(s"Failed to parse JSON from AttributeValue", e))
          a <- json.as[A](jsonCodec)
            .left.map(e => DynamodecDecodeError(s"Failed to decode JSON from AttributeValue", e))
        yield a
