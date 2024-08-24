package com.adamnfish.dynamodecs.integration

import software.amazon.awssdk.auth.credentials.{AwsBasicCredentials, StaticCredentialsProvider}
import software.amazon.awssdk.core.client.config.ClientOverrideConfiguration
import software.amazon.awssdk.regions.Region
import software.amazon.awssdk.services.dynamodb.DynamoDbClient
import software.amazon.awssdk.services.dynamodb.model.{AttributeDefinition, KeySchemaElement, ScalarAttributeType}

import java.net.URI
import java.util.UUID
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.jdk.DurationConverters.*


object Helpers:
  def syncDbClient(port: Int = 8042): DynamoDbClient =
    DynamoDbClient.builder
      .endpointOverride(URI.create(s"http://localhost:$port"))
      .overrideConfiguration(
        ClientOverrideConfiguration.builder
          .apiCallAttemptTimeout(5.seconds.toJava)
          .apiCallTimeout(5.seconds.toJava)
          .build
      )
      // these credentials are ignored locally, but need to be provided
      .credentialsProvider(StaticCredentialsProvider.create(AwsBasicCredentials.create("dummy", "credentials")))
      // the region is ignored locally, but needs to be provided
      .region(Region.EU_WEST_1)
      .build

  /**
   * Helper function to create a table for a test and clean it up afterwards.
   *
   * The table name will be randomly generated so that table names never clash during parallel execution.
   *
   * @param client DynamoDbClient to use
   * @param hashKey the hash key for the table with its type
   * @param rangeKey Optionally, the range key for the table with its type
   */
  def withRandomDbTable[T](client: DynamoDbClient, hashKey: (String, ScalarAttributeType), rangeKey: Option[(String, ScalarAttributeType)])(f: String => T): T =
    val allAttrs = (Some(hashKey) ++ rangeKey)
      .map:
        case (label, attrType) => AttributeDefinition.builder.attributeName(label).attributeType(attrType).build
      .toList
      .asJava
    val hashKeySchema = KeySchemaElement.builder.keyType("HASH").attributeName(hashKey._1).build
    val tableName = UUID.randomUUID().toString

    client.createTable: builder =>
      builder
        .tableName(tableName)
        .attributeDefinitions(allAttrs)
        .keySchema(
          rangeKey match
            case Some((label, _)) =>
              List(
                hashKeySchema,
                KeySchemaElement.builder.keyType("RANGE").attributeName(label).build
              ).asJava
            case None =>
              List(
                hashKeySchema
              ).asJava
        )
        // this is ignored locally, but needs to be provided
        .billingMode("PAY_PER_REQUEST")

    try
      f(tableName)
    finally
      client.deleteTable: builder =>
        builder.tableName(tableName)
