package aws.cfn.specification

import argonaut.{DecodeJson, Json}
import aws.cfn.specification.maps.{InterResourceReferencesMap, PolicyReferencesMap}
import aws.cfn.formalization._

import scala.language.postfixOps





object JsonDecoder {

  def decodeWithName(json:Json, resSpecFileName:String): ResourceSpecification = {

    val resSpecName = Renaming.resourceSpecificationName(resSpecFileName)
    val resourceTypeNode = DecodUtil.getNodesAsMap( json.field("ResourceType").getOrElse(json.field("ResourceTypes").get) ).toList
    val subpropertyTypeNodes = DecodUtil.getNodesAsMap( json.field("PropertyTypes").getOrElse(Json.jEmptyObject) )

    new ResourceSpecification(
      resSpecName,
      ResourceTypeDecoder.decodeWithName( resourceTypeNode.head._2, resourceTypeNode.head._1, resSpecName) ,
      subpropertyTypeNodes.toVector map ( n => SubpropertyTypeDecoder.decodeWithName(n._2, n._1, resSpecName) )
    )

  }

}










private object ResourceTypeDecoder {

  def decodeWithName(json:Json, restypeName:String, resSpecName:String):ResourceValueType = {
    val resource = ResourceValueType(Renaming.resourceTypeName(restypeName))
    resource.addPropertyList( PropertiesDecoder.decodeWithDomain(json, resource, resSpecName) )
    resource.addAttributeList( AttributesDecoder.decodeWithDomain(json, resource) )
    resource
  }

}










private object SubpropertyTypeDecoder {

  def decodeWithName(json:Json, subpropName:String,resSpecName:String):SubPropertyValueType = {
    val subProperty = SubPropertyValueType(Renaming.subpropertyTypeName(subpropName))
    subProperty.addPropertyList( PropertiesDecoder.decodeWithDomain(json, subProperty, resSpecName) )
    subProperty
  }

}










private object PropertiesDecoder {

  def decodeWithDomain(json: Json, domain: NonPrimitiveValueType,resSpecName:String) : Vector[GenericPropertyType] = {

    def toListOfProperties(propNodesMap: Map[String,Json])  = propNodesMap.toList map toPropertyType

    def isReq(j:Json) = DecodeJson.BooleanDecodeJson.decodeJson( j.field("Required").getOrElse(
      Json.jBool(false)) ).toOption.get
    def isFun(j:Json) = !(DecodUtil.isList(j) || DecodUtil.isMap(j))

    def toPropertyType(pair: (String,Json)) : GenericPropertyType = {

      val propName = Renaming.propertyName(domain.name,pair._1)
      val unambPropName = Renaming.unambiguousPropertyName( resSpecName, domain.name, pair._1)
      val isInterResourceReference = InterResourceReferencesMap.lookUp(unambPropName)
      val isPolicyDocument = PolicyReferencesMap.lookup(unambPropName)

      if ( isInterResourceReference.isEmpty && isPolicyDocument.isEmpty )
      {

        if (DecodUtil.isList(pair._2) && DecodUtil.ofPrimitive(pair._2))
          ListOfPrimitiveProperty( DecodUtil.getLowerCaseStringField(pair._2,"PrimitiveItemType"), propName, domain, isReq(pair._2))
        else if (DecodUtil.isList(pair._2) && !DecodUtil.ofPrimitive(pair._2))
          ListOfNonPrimitiveProperty( DecodUtil.getLowerCaseStringField(pair._2,"ItemType") , propName, domain, isReq(pair._2))
        else if (DecodUtil.isMap(pair._2) && DecodUtil.ofPrimitive(pair._2))
          MapOfPrimitiveProperty( DecodUtil.getLowerCaseStringField(pair._2,"PrimitiveItemType") , propName, domain, isReq(pair._2))
        else if (DecodUtil.isMap(pair._2) && !DecodUtil.ofPrimitive(pair._2))
          MapOfNonPrimitiveProperty( DecodUtil.getLowerCaseStringField(pair._2,"ItemType") , propName, domain, isReq(pair._2))
        else if (DecodUtil.isComplexType(pair._2))
          SubpropertyProperty(DecodUtil.getLowerCaseStringField(pair._2, "Type"), propName, domain, isReq(pair._2))
        else DecodUtil.getPrimitiveType(pair._2) match {
          case "string" => StringProperty(propName, domain, isReq(pair._2))
          case "float" => FloatProperty(propName, domain, isReq(pair._2))
          case "long" => LongProperty(propName, domain, isReq(pair._2))
          case "integer" => IntProperty(propName, domain, isReq(pair._2))
          case "boolean" => BooleanProperty(propName, domain, isReq(pair._2))
          case "datetime" => DateTimeProperty(propName, domain, isReq(pair._2))
          case "commadelimitedlist" => CommaDelimitedListProperty(propName, domain, isReq(pair._2))
          case "json" => JsonProperty(propName, domain, isReq(pair._2))
          case _ => StringProperty(propName, domain, isReq(pair._2))
        }

      }
      else if (isInterResourceReference.isDefined)
      {
        val x = isInterResourceReference.get
        ResourceProperty(x._1,x._2, Renaming.propertyName(domain.name,pair._1), domain, x._3)
      }
      else // if !isPolicyDocument.isEmpty
      {
        PolicyProperty(propName, domain, isReq(pair._2))
      }

    }


    toListOfProperties( DecodUtil.getNodesAsMap(json.field("Properties").getOrElse(Json.jEmptyObject)) ).toVector
  }

}









private object AttributesDecoder{

  def decodeWithDomain(json:Json, domain:ResourceValueType) : Vector[GenericAttributeType] = {

    def mapToList(attrNodesMap: Map[String,Json]) = attrNodesMap.toList map toAttributeType

    def toAttributeType ( pair: (String,Json) ) : GenericAttributeType = {

      val attrName = Renaming.attributeName(domain.name,pair._1)

      if (DecodUtil.isList(pair._2) && DecodUtil.ofPrimitive(pair._2))
        ListOfPrimitiveAttribute(DecodUtil.getLowerCaseStringField(pair._2, "PrimitiveItemType"), attrName, domain)
      else DecodUtil.getPrimitiveType(pair._2) match {
        case "string" => StringAttribute(attrName, domain)
        case "float" => FloatAttribute(attrName, domain)
        case "long" => LongAttribute(attrName, domain)
        case "integer" => IntAttribute(attrName, domain)
        case "boolean" => BooleanAttribute(attrName, domain)
        case "datetime" => DateTimeAttribute(attrName, domain)
        case "commadelimitedlist" => CommaDelimitedListAttribute(attrName, domain)
        case "json" => JsonAttribute(attrName, domain)
        case _ => StringAttribute(attrName, domain)
      }
    }

    mapToList( DecodUtil.getNodesAsMap(json.field("Attributes").getOrElse(Json.jEmptyObject)) ).toVector
  }

}










private object DecodUtil{

  def getNodesAsMap(j:Json) : Map[String, Json] = subFieldNames(j) zip subFieldContents(j) toMap

  def isList( json : Json ):Boolean = json.hasField("Type") && getLowerCaseStringField(json,"Type").equals("list")
  def ofPrimitive(json: Json):Boolean = json.hasField("PrimitiveItemType")
  def isMap( json : Json ):Boolean = json.hasField("Type") && getLowerCaseStringField(json,"Type").equals("map")
  def isComplexType (json: Json):Boolean = json.hasField("Type")
  def getPrimitiveType ( json : Json ):Any = if (json.hasField("PrimitiveType")) getLowerCaseStringField(json,"PrimitiveType")

  def subFieldNames (json : Json) : List[String] = json.objectFields.get map (f => f.toString)
  def subFieldContents (json : Json) : List[Json] = json.objectFieldsOrEmpty map (f => json.fieldOrEmptyArray(f) )

  def getLowerCaseStringField(json: Json, field:String): String =
    DecodeJson.StringDecodeJson.decodeJson(json.field(field).get).toOption.get.toLowerCase()

}