package com.gu.dynamodecs.attributes

import com.gu.dynamodecs.DynamodecResult


trait MapKeyCodec[A]:
  def encodeKey(a: A): String
  def decodeKey(str: String): DynamodecResult[A]

object MapKeyCodec:
  def apply[A: MapKeyCodec]: MapKeyCodec[A] =
    summon[MapKeyCodec[A]]

  // create a custom instance

  def instance[A](encode: A => String)(decode: String => DynamodecResult[A]): MapKeyCodec[A] =
    new MapKeyCodec[A] {
      def encodeKey(a: A): String = encode(a)
      def decodeKey(str: String): DynamodecResult[A] = decode(str)
    }

  // default instances

  given MapKeyCodec[String] with
    def encodeKey(a: String): String = a
    def decodeKey(str: String): DynamodecResult[String] =
      Right(str)

  given MapKeyCodec[Int] with
    def encodeKey(a: Int): String = a.toString
    def decodeKey(str: String): DynamodecResult[Int] =
      Right(str.toInt)

  given MapKeyCodec[Long] with
    def encodeKey(a: Long): String = a.toString
    def decodeKey(str: String): DynamodecResult[Long] =
      Right(str.toLong)

  given MapKeyCodec[Short] with
    def encodeKey(a: Short): String = a.toString
    def decodeKey(str: String): DynamodecResult[Short] =
      Right(str.toShort)

  given MapKeyCodec[Char] with
    def encodeKey(a: Char): String = a.toString
    def decodeKey(str: String): DynamodecResult[Char] =
      Right(str.head)

  given MapKeyCodec[Byte] with
    def encodeKey(a: Byte): String = a.toString
    def decodeKey(str: String): DynamodecResult[Byte] =
      Right(str.toByte)

  given MapKeyCodec[Boolean] with
    def encodeKey(a: Boolean): String = a.toString
    def decodeKey(str: String): DynamodecResult[Boolean] =
      Right(str.toBoolean)

  given MapKeyCodec[Float] with
    def encodeKey(a: Float): String = a.toString
    def decodeKey(str: String): DynamodecResult[Float] =
      Right(str.toFloat)

  given MapKeyCodec[Double] with
    def encodeKey(a: Double): String = a.toString
    def decodeKey(str: String): DynamodecResult[Double] =
      Right(str.toDouble)

  given MapKeyCodec[BigInt] with
    def encodeKey(a: BigInt): String = a.toString
    def decodeKey(str: String): DynamodecResult[BigInt] =
      Right(BigInt(str))
