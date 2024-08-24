Dynamodecs
==========

Derive DynamoDB codecs, for Scala 3 projects.

Dynamodecs makes it easy to serialize and deserialize Scala case classes to and from DynamoDB items.
It does not attempt to replace the AWS DynamoDB SDK, but focuses on providing an easy way to serialise
your datastructures for storage and retrieval in DynamoDB.

WIP: Not yet published

## Usage

The tests and especially the integration tests give worked examples of how to use Dynamodecs.

### Basic usage

```scala
import com.gu.dynamodecs.*
import software.amazon.awssdk.services.dynamodb.DynamoDbClient

// Your configured DyanmoDB client
val dbClient: DynamoDbClient = ???

// Your datastructure
case class DbData(id: String, description: String, count: Int)
val dbData = DbData("1234", "A nice description", 42)

// Save and retrieve data without manual serialization
dbClient.putItem(
  PutItemRequest.builder()
    .tableName(tableName)
    .item(dbData.asJavaDbItem) // This will automatically encode our data
    .build()
)
val response = dbClient.getItem(
  GetItemRequest.builder()
    .tableName(tableName)
    .key(Map("id" -> AttributeCodec.encode(dbData.id)).asJava) // This will encode our key
    .build()
)
// This will attempt to automatically decode our database item data into a DbData instance
val result: DynamodecResult[DbData] = response.item().fromJavaDbItem[DbData]
```

Decoding the data we retrieve from the DB might fail. If the data is not in the expected format, we won't be able
to create an instance of `DbData`. Because of this the `result` is a `DynamodecResult`, which is an Either that
contains the deserialized data if it succeeded, or an error message if the data was not in the expected format.

This is the same approach that libraries like [circe](https://circe.github.io/circe/) use to handle decoding errors.

### Nested case classes

Dynamodecs supports nested case classes by using a JSON Codec to encode and decode the nested data structure.

For example, given the following case classes:

```scala
import com.gu.dynamodecs.*
import io.circe
import software.amazon.awssdk.services.dynamodb.DynamoDbClient

// Your configured DyanmoDB client
val dbClient: DynamoDbClient = ???

case class NestedData(description: String, count: Int)
// We encode nested case classes using a JSON codec, so this must be made available
given circe.Codec[NestedData] = deriveCodec

case class DbData(id: String, nested: NestedData)
val dbData = DbData("1234", NestedData("nested-information", 52))

dbClient.putItem(
  PutItemRequest.builder()
    .tableName(tableName)
    .item(dbData.asJavaDbItem) // This will automatically encode our data, using the JSON codec for the nested data
    .build()
)
val getItemResponse = dbClient.getItem(
  GetItemRequest.builder()
    .tableName(tableName)
    .key(Map("id" -> AttributeCodec.encode(dbData.id)).asJava) // This will encode our key
    .build()
)
// This will attempt to automatically decode our database item into a DbData instance
val result: DynamodecResult[DbData] = getItemResponse.item().fromJavaDbItem[DbData]
```
