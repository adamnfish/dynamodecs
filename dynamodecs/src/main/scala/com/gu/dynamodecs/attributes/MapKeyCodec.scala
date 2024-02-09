package com.gu.dynamodecs.attributes

import com.gu.dynamodecs.{DynamodecDecodeError, DynamodecResult}

import scala.util.Try


trait MapKeyCodec[A]:
  def encodeKey(a: A): String
  def decodeKey(str: String): DynamodecResult[A]

object MapKeyCodec:
  def apply[A: MapKeyCodec]: MapKeyCodec[A] =
    summon[MapKeyCodec[A]]

  // create a custom instance

  def instance[A](decode: String => A)(encode: A => String): MapKeyCodec[A] =
    new MapKeyCodec[A] {
      def encodeKey(a: A): String = encode(a)
      def decodeKey(str: String): DynamodecResult[A] = Right(decode(str))
    }

  def instanceE[A](decode: String => DynamodecResult[A])(encode: A => String): MapKeyCodec[A] =
    new MapKeyCodec[A] {
      def encodeKey(a: A): String = encode(a)
      def decodeKey(str: String): DynamodecResult[A] = decode(str)
    }

  extension [A](mkc: MapKeyCodec[A])
    def imap[B](f: A => B)(g: B => A): MapKeyCodec[B] =
      new MapKeyCodec[B]:
        override def encodeKey(b: B): String = mkc.encodeKey(g(b))
        override def decodeKey(str: String): DynamodecResult[B] = mkc.decodeKey(str).map(f)

    def iemap[B](f: A => DynamodecResult[B])(g: B => A): MapKeyCodec[B] =
      new MapKeyCodec[B]:
        override def encodeKey(b: B): String = mkc.encodeKey(g(b))
        override def decodeKey(str: String): DynamodecResult[B] = mkc.decodeKey(str).flatMap(f)

  // default instances

  given MapKeyCodec[String] with
    def encodeKey(a: String): String = a
    def decodeKey(str: String): DynamodecResult[String] =
      // TODO: how does this fail?
      Right(str)

  given MapKeyCodec[Int] with
    def encodeKey(a: Int): String = a.toString
    def decodeKey(str: String): DynamodecResult[Int] =
      str.toIntOption.toRight(DynamodecDecodeError(s"Expected an integer for map key, got: $str"))

  given MapKeyCodec[Long] with
    def encodeKey(a: Long): String = a.toString
    def decodeKey(str: String): DynamodecResult[Long] =
      str.toLongOption.toRight(DynamodecDecodeError(s"Expected a long for map key, got: $str"))

  given MapKeyCodec[Short] with
    def encodeKey(a: Short): String = a.toString
    def decodeKey(str: String): DynamodecResult[Short] =
      // TODO: how does this fail?
      Right(str.toShort)

  given MapKeyCodec[Char] with
    def encodeKey(a: Char): String = a.toString
    def decodeKey(str: String): DynamodecResult[Char] =
      if (str.length != 1)
        Left(DynamodecDecodeError(s"Expected a single character for map key, got: $str"))
      else
        Right(str.head)

  // TODO: think about what expectations for this should be? Consider prior art
//  given MapKeyCodec[Byte] with
//    def encodeKey(a: Byte): String = a.toString
//    def decodeKey(str: String): DynamodecResult[Byte] = ???

  given MapKeyCodec[Boolean] with
    def encodeKey(a: Boolean): String = a.toString
    def decodeKey(str: String): DynamodecResult[Boolean] =
      str.toBooleanOption.toRight(DynamodecDecodeError(s"Expected a boolean for map key, got: $str"))

  given MapKeyCodec[Float] with
    def encodeKey(a: Float): String = a.toString
    def decodeKey(str: String): DynamodecResult[Float] =
      str.toFloatOption.toRight(DynamodecDecodeError(s"Expected a float for map key, got: $str"))

  given MapKeyCodec[Double] with
    def encodeKey(a: Double): String = a.toString
    def decodeKey(str: String): DynamodecResult[Double] =
      str.toDoubleOption.toRight(DynamodecDecodeError(s"Expected a double for map key, got: $str"))

  given MapKeyCodec[BigInt] with
    def encodeKey(a: BigInt): String = a.toString
    def decodeKey(str: String): DynamodecResult[BigInt] =
      Try(BigInt(str)).toEither.left.map(err => DynamodecDecodeError(s"Expected a BigInt for map key, got: $str", err))
