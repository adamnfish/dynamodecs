package com.gu.dynamodecs.attributes

import com.gu.dynamodecs.DynamodecResult
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

import scala.compiletime.{constValue, erasedValue, summonInline}
import scala.deriving.Mirror


trait UnwrappedAttributeCodec[A] extends AttributeCodec[A]:
  def encode(a: A): AttributeValue
  def decode(av: AttributeValue): DynamodecResult[A]

object UnwrappedAttributeCodec:
  def apply[A : UnwrappedAttributeCodec]: UnwrappedAttributeCodec[A] = summon[UnwrappedAttributeCodec[A]]


  inline def deriveUnwrapper[A <: Product](using m: Mirror.ProductOf[A]): UnwrappedAttributeCodec[A] =
    val codec = getSingleCodec[m.MirroredElemTypes]
    new UnwrappedAttributeCodec[A]:
      override def encode(a: A): AttributeValue =
        codec.encode(a.productIterator.next())
      override def decode(av: AttributeValue): DynamodecResult[A] =
        codec.decode(av).map(a => m.fromProduct(Tuple1(a)))

  private inline def getSingleCodec[T <: Tuple]: AttributeCodec[Any] =
    inline erasedValue[T] match
      case _: EmptyTuple =>
        compiletime.error("UnwrappedAttributeCodec.deriveUnwrapper only supports case classes with a single field (found 0 fields)")
      case _: (thisType *: EmptyTuple) =>
        summonInline[AttributeCodec[thisType]].asInstanceOf[AttributeCodec[Any]]
      case _ =>
        compiletime.error("UnwrappedAttributeCodec.deriveUnwrapper only supports case classes with a single field")
