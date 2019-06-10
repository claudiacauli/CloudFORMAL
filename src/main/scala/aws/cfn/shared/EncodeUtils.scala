package aws.cfn.shared

import argonaut.Json
import scala.language.postfixOps

object EncodeUtils {


  def getNodesAsMapOfJsons(j:Json) : Map[String, Json] =
    subFieldNames(j) zip subFieldContents(j) toMap

  def getNodesAsMapOfStrings(j:Json) : Map[String, String] =
    subFieldNames(j) zip subFieldValueContents(j) toMap

  def isList(j : Json ) : Boolean =
    j.hasField("Type") && getLowerCaseStringField(j,"Type").equals("list")

  def ofPrimitive(j: Json) : Boolean =
    j.hasField("PrimitiveItemType")

  def isMap(j : Json ) : Boolean =
    j.hasField("Type") && getLowerCaseStringField(j,"Type").equals("map")

  def isComplexType (j: Json) : Boolean =
    j.hasField("Type")

  def getPrimitiveType (j : Json ) : Option[String] =
    if (j.hasField("PrimitiveType"))
      Some(getLowerCaseStringField(j,"PrimitiveType"))
    else
      None

  def subFieldNames (j : Json) : List[String] =
    j.objectFields.get map (f => f.toString.toLowerCase)

  def subFieldContents (j : Json) : List[Json] =
    j.objectFieldsOrEmpty map (f => j.fieldOrEmptyArray(f) )

  def subFieldValueContents (j: Json) : List[String] =
    j.objectFieldsOrEmpty map (f => getLowerCaseStringField(j, f) )

  def getLowerCaseStringField(j: Json, field:String): String =
    j.field(field).get.string.get.toLowerCase()


}