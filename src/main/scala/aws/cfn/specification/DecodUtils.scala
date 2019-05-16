package aws.cfn.specification

import argonaut.{DecodeJson, Json}

object DecodUtils {

  def getNodesAsMapOfJsons(j:Json) : Map[String, Json] = subFieldNames(j) zip subFieldContents(j) toMap

  def getNodesAsMapOfStrings(j:Json) : Map[String, String] = subFieldNames(j) zip subFieldValueContents(j) toMap

  def isList( json : Json ):Boolean = json.hasField("Type") && getLowerCaseStringField(json,"Type").equals("list")

  def ofPrimitive(json: Json):Boolean = json.hasField("PrimitiveItemType")

  def isMap( json : Json ):Boolean = json.hasField("Type") && getLowerCaseStringField(json,"Type").equals("map")

  def isComplexType (json: Json):Boolean = json.hasField("Type")

  def getPrimitiveType ( json : Json ):Any = if (json.hasField("PrimitiveType")) getLowerCaseStringField(json,"PrimitiveType")

  def subFieldNames (json : Json) : List[String] = json.objectFields.get map (f => f.toString.toLowerCase)

  def subFieldContents (json : Json) : List[Json] = json.objectFieldsOrEmpty map (f => json.fieldOrEmptyArray(f) )

  def subFieldValueContents (json: Json) : List[String] =
    json.objectFieldsOrEmpty map ( f => getLowerCaseStringField(json, f) )

  def getLowerCaseStringField(json: Json, field:String): String =
    DecodeJson.StringDecodeJson.decodeJson(json.field(field).get).toOption.get.toLowerCase()

}
