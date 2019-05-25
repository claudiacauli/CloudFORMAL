package aws.cfn.encoding.specification

import argonaut.{DecodeJson, Json}
import aws.cfn.formalization._
import aws.cfn.encoding.{EncodeUtils, Renaming}
import aws.cfn.encoding.specification.maps.{InterResourceReferencesMap, PolicyReferencesMap}

import scala.language.postfixOps


object Json2SpecificationEncoder {

  def encode(json: Json, resourceSpecificationName: String) : ResourceSpecification =
    new Json2SpecificationEncoder(json, resourceSpecificationName).encode()

}



private class Json2SpecificationEncoder(json: Json, resourceSpecificationName: String) {

  val resSpecName: String = Renaming.resourceSpecificationName(resourceSpecificationName)

  protected def encode(): ResourceSpecification = {

    val resourceTypeNode: Vector[(String, Json)] = EncodeUtils.getNodesAsMapOfJsons( json.field("ResourceType")./*getOrElse(json.field("ResourceTypes").*/get) .toVector
    val subpropertyTypeNodes: Vector[(String, Json)] = EncodeUtils.getNodesAsMapOfJsons( json.field("PropertyTypes").getOrElse(Json.jEmptyObject) ).toVector

    new ResourceSpecification(
      resSpecName,
      encodeResourceTypeWithName( resourceTypeNode.head._2, resourceTypeNode.head._1) ,
      subpropertyTypeNodes map ( n => encodeSubpropertyTypeWithName(n._2, n._1) )
    )

  }






  private def encodeResourceTypeWithName(json:Json, restypeName:String):ResourceValueType = {
    val resource = ResourceValueType(Renaming.resourceTypeName(restypeName))
    resource.addPropertyList( encodePropertiesWithDomain(json, resource) )
    resource.addAttributeList( encodeAttributesWithDomain(json, resource) )
    resource
  }






  private def encodeSubpropertyTypeWithName(json:Json, subpropName:String):SubPropertyValueType = {
    val subProperty = SubPropertyValueType(Renaming.subpropertyTypeName(subpropName))
    subProperty.addPropertyList( encodePropertiesWithDomain(json, subProperty) )
    subProperty
  }






  private def encodePropertiesWithDomain(json: Json, domain: NonPrimitiveValueType) : Vector[GenericPropertyType] = {


    def toPropertyType(pair: (String,Json)) : GenericPropertyType = {

      val propName = Renaming.propertyName(domain.name,pair._1)
      val unambPropName = Renaming.unambiguousPropertyName( resSpecName, domain.name, pair._1)
      val isInterResourceReference = InterResourceReferencesMap.lookUp(unambPropName)
      val isPolicyDocument = PolicyReferencesMap.lookup(unambPropName)

      if ( isInterResourceReference.isEmpty && isPolicyDocument.isEmpty )
      {

        if (EncodeUtils.isList(pair._2) && EncodeUtils.ofPrimitive(pair._2))
          ListOfPrimitiveProperty( EncodeUtils.getLowerCaseStringField(pair._2,"PrimitiveItemType"), propName, domain, isReq(pair._2))
        else if (EncodeUtils.isList(pair._2) && !EncodeUtils.ofPrimitive(pair._2))
          ListOfNonPrimitiveProperty( EncodeUtils.getLowerCaseStringField(pair._2,"ItemType") , propName, domain, isReq(pair._2))
        else if (EncodeUtils.isMap(pair._2) && EncodeUtils.ofPrimitive(pair._2))
          MapOfPrimitiveProperty( EncodeUtils.getLowerCaseStringField(pair._2,"PrimitiveItemType") , propName, domain, isReq(pair._2))
        else if (EncodeUtils.isMap(pair._2) && !EncodeUtils.ofPrimitive(pair._2))
          MapOfNonPrimitiveProperty( EncodeUtils.getLowerCaseStringField(pair._2,"ItemType") , propName, domain, isReq(pair._2))
        else if (EncodeUtils.isComplexType(pair._2))
          SubpropertyProperty(EncodeUtils.getLowerCaseStringField(pair._2, "Type"), propName, domain, isReq(pair._2))
        else EncodeUtils.getPrimitiveType(pair._2) match {
          case "string" => StringProperty(propName, domain, isReq(pair._2))
          case "float" => FloatProperty(propName, domain, isReq(pair._2))
          case "double" => DoubleProperty(propName, domain, isReq(pair._2))
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
        if ( x._4 )
          ResourceProperty(x._1,x._2, Renaming.propertyName(domain.name,pair._1), domain, x._3)
        else
          ListOfResourcesProperty(x._1,x._2, Renaming.propertyName(domain.name, pair._1), domain, x._3)

      }
      else // if !isPolicyDocument.isEmpty
      {
        PolicyProperty(propName, domain, isReq(pair._2))
      }

    }

    toListOf( EncodeUtils.getNodesAsMapOfJsons(json.field("Properties").getOrElse(Json.jEmptyObject)) , toPropertyType)

  }






  private def encodeAttributesWithDomain(json:Json, domain:ResourceValueType) : Vector[GenericAttributeType] = {

    def toAttributeType ( pair: (String,Json) ) : GenericAttributeType = {

      val attrName = Renaming.attributeName(domain.name,pair._1)

      if (EncodeUtils.isList(pair._2) && EncodeUtils.ofPrimitive(pair._2))
        ListOfPrimitiveAttribute(EncodeUtils.getLowerCaseStringField(pair._2, "PrimitiveItemType"), attrName, domain)
      else EncodeUtils.getPrimitiveType(pair._2) match {
        case "string" => StringAttribute(attrName, domain)
        case "float" => FloatAttribute(attrName, domain)
        case "double" => DoubleAttribute(attrName, domain)
        case "long" => LongAttribute(attrName, domain)
        case "integer" => IntAttribute(attrName, domain)
        case "boolean" => BooleanAttribute(attrName, domain)
        case "datetime" => DateTimeAttribute(attrName, domain)
        case "commadelimitedlist" => CommaDelimitedListAttribute(attrName, domain)
        case "json" => JsonAttribute(attrName, domain)
        case _ => StringAttribute(attrName, domain)
      }
    }

    toListOf( EncodeUtils.getNodesAsMapOfJsons(json.field("Attributes").getOrElse(Json.jEmptyObject)) , toAttributeType )

  }





  private def toListOf [T <: GenericAttributeOrPropertyType] (nodesMap: Map[String,Json] , f: ((String,Json)) => T ) : Vector[T]  =
    nodesMap.toVector map f

  private def isReq(j:Json): Boolean = DecodeJson.BooleanDecodeJson.decodeJson( j.field("Required").getOrElse(
    Json.jBool(false)) ).toOption.get


}