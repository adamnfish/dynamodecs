package com.gu.dynamodecs.items

import com.gu.dynamodecs.DynamodecResult
import com.gu.dynamodecs.attributes.AttributeCodec
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

import scala.compiletime.{constValue, erasedValue, summonInline}
import scala.deriving.Mirror

import cats.*
import cats.implicits.*


trait ItemCodec[A]:
  def encode(a: A): Map[String, AttributeValue]
  def decode(item: Map[String, AttributeValue]): DynamodecResult[A]

object ItemCodec:
  def apply[A : ItemCodec]: ItemCodec[A] =
    summon[ItemCodec[A]]

  extension [A : ItemCodec](a: A)
    def asDbItem: Map[String, AttributeValue] =
      summon[ItemCodec[A]].encode(a)

  extension (item: Map[String, AttributeValue])
    def fromDbItem[A : ItemCodec]: DynamodecResult[A] =
      summon[ItemCodec[A]].decode(item)

  extension [A](avc: ItemCodec[A])
    def imap[B](f: A => B)(g: B => A): ItemCodec[B] =
      new ItemCodec[B]:
        override def encode(b: B): Map[String, AttributeValue] =
          avc.encode(g(b))
        override def decode(av: Map[String, AttributeValue]): DynamodecResult[B] =
          avc.decode(av).map(f)

  def instance[A](encodeItem: A => Map[String, AttributeValue])(decodeItem: Map[String, AttributeValue] => A): ItemCodec[A] =
    new ItemCodec[A]:
      override def encode(a: A): Map[String, AttributeValue] =
        encodeItem(a)
      override def decode(item: Map[String, AttributeValue]): DynamodecResult[A] =
        Right(decodeItem(item))

  inline given derived[A <: Product](using m: Mirror.ProductOf[A]): ItemCodec[A] =
    val labelsAndCodecs = getLabelsAndCodecs[m.MirroredElemLabels, m.MirroredElemTypes]

    new ItemCodec[A]:
      override def encode(t: A): Map[String, AttributeValue] =
        t.productIterator.zip(labelsAndCodecs)
          .map { case (value, (label, codec)) =>
            label -> codec.encode(value)
          }
          .toMap

      override def decode(map: Map[String, AttributeValue]): DynamodecResult[A] =
        val values = labelsAndCodecs
          .traverse { (label, codec) =>
            codec.decode(map(label))
          }
        values.map(aa => m.fromProduct(Tuple.fromArray(aa.toArray)))

  private inline def getLabelsAndCodecs[L <: Tuple, T <: Tuple]: List[(String, AttributeCodec[Any])] =
    inline (erasedValue[L], erasedValue[T]) match
      case (_: EmptyTuple, _: EmptyTuple) =>
        Nil
      case (_: (thisLabel *: labels), _: (thisType *: types)) =>
        val attributeLabel = constValue[thisLabel].toString
        val attributeCodec = summonInline[AttributeCodec[thisType]].asInstanceOf[AttributeCodec[Any]]
        (attributeLabel, attributeCodec) :: getLabelsAndCodecs[labels, types]
