package com.adamnfish.dynamodecs.integration

import com.adamnfish.dynamodecs.attributes.AttributeCodec
import com.adamnfish.dynamodecs.items.ItemCodec
import org.scalatest.freespec.AnyFreeSpec
import software.amazon.awssdk.services.dynamodb.DynamoDbClient
import software.amazon.awssdk.services.dynamodb.model.{GetItemRequest, PutItemRequest, ScalarAttributeType}
import ItemCodec.*
import io.circe
import io.circe.generic.semiauto.deriveCodec
import org.scalatest.matchers.should.Matchers

import scala.jdk.CollectionConverters.*


class SimpleExampleTest extends AnyFreeSpec with Matchers:
  val dbClient: DynamoDbClient = Helpers.syncDbClient()

  "Simple example" in {
    case class Data(id: String, value: String)

    Helpers.withRandomDbTable(dbClient, hashKey = ("id", ScalarAttributeType.S), rangeKey = None): tableName =>
      val data = Data("1", "hello")
      dbClient.putItem(
        PutItemRequest.builder()
          .tableName(tableName)
          .item(data.asJavaDbItem)
          .build()
      )
      val getItemResponse = dbClient.getItem(
        GetItemRequest.builder()
          .tableName(tableName)
          .key(Map("id" -> AttributeCodec.encode(data.id)).asJava)
          .build()
      )
      val result = getItemResponse.item().fromJavaDbItem[Data]
      result shouldEqual Right(data)
  }

  "Simple example with nested case class" in {
    case class NestedData(id: String, value: Int)
    // We encode nested case classes using a JSON codec, so this must be made available
    given circe.Codec[NestedData] = deriveCodec
    case class Data(id: String, nested: NestedData) derives ItemCodec

    Helpers.withRandomDbTable(dbClient, hashKey = ("id", ScalarAttributeType.S), rangeKey = None): tableName =>
      val data = Data("1", NestedData("nested-information", 1234))
      dbClient.putItem(
        PutItemRequest.builder()
          .tableName(tableName)
          .item(data.asJavaDbItem)
          .build()
      )
      val getItemResponse = dbClient.getItem(
        GetItemRequest.builder()
          .tableName(tableName)
          .key(Map("id" -> AttributeCodec.encode(data.id)).asJava)
          .build()
      )
      val result = getItemResponse.item().fromJavaDbItem[Data]
      result shouldEqual Right(data)
  }
