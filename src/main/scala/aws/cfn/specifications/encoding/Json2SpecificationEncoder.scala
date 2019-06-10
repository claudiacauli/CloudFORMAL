package aws.cfn.specifications.encoding

import argonaut.{DecodeJson, Json}
import aws.cfn.shared.{EncodeUtils, SpecificationRenaming}
import aws.cfn.maps.{InterResourceReferencesMap, PolicyDocumentsReferencesMap}
import aws.cfn.specifications.formalization._

import scala.language.postfixOps


object Json2SpecificationEncoder {

  def encode(json: Json, resourceSpecificationName: String) : ResourceSpecification =
    new Json2SpecificationEncoder(json, resourceSpecificationName).encode()

}



private class Json2SpecificationEncoder(json: Json, resourceSpecificationName: String) {

  val resSpecName: String = SpecificationRenaming.resourceSpecificationName(resourceSpecificationName)

  protected def encode(): ResourceSpecification = {

    val resourceTypeNode: Vector[(String, Json)] =
      EncodeUtils.getNodesAsMapOfJsons( json.field("ResourceType").get) .toVector
    val subpropertyTypeNodes: Vector[(String, Json)] =
      EncodeUtils.getNodesAsMapOfJsons( json.field("PropertyTypes").getOrElse(Json.jEmptyObject) ).toVector

    new ResourceSpecification(
      resSpecName,
      encodeResourceTypeWithName( resourceTypeNode.head._2, resourceTypeNode.head._1) ,
      subpropertyTypeNodes map ( n => encodeSubpropertyTypeWithName(n._2, n._1) )
    )

  }






  private def encodeResourceTypeWithName(json:Json, restypeName:String):ResourceValueType = {
    val resource = ResourceValueType(SpecificationRenaming.resourceTypeName(restypeName))
    resource.addPropertyList( encodePropertiesWithDomain(json, resource) )
    resource.addAttributeList( encodeAttributesWithDomain(json, resource) )
    resource
  }






  private def encodeSubpropertyTypeWithName(json:Json, subpropName:String):SubPropertyValueType = {
    val subProperty = SubPropertyValueType(SpecificationRenaming.subpropertyTypeName(subpropName))
    subProperty.addPropertyList( encodePropertiesWithDomain(json, subProperty) )
    subProperty
  }






  private def encodePropertiesWithDomain(json: Json, domain: NonPrimitiveValueType) : Vector[GenericPropertyType] = {


    def toPropertyType(pair: (String,Json)) : GenericPropertyType = {

      val propName = SpecificationRenaming.propertyName(domain.name,pair._1)
      val unambPropName = SpecificationRenaming.unambiguousPropertyName( resSpecName, domain.name, pair._1)
      val isInterResourceReference = InterResourceReferencesMap.lookUp(unambPropName)
      val isPolicyDocument = PolicyDocumentsReferencesMap.lookup(unambPropName)
      def primitiveItem = EncodeUtils.getLowerCaseStringField(pair._2,"PrimitiveItemType")
      def item = EncodeUtils.getLowerCaseStringField(pair._2,"ItemType")

      if ( isInterResourceReference.isEmpty && isPolicyDocument.isEmpty )
      {

        if (EncodeUtils.isList(pair._2) && EncodeUtils.ofPrimitive(pair._2))
          ListOfPrimitiveProperty( primitiveItem, propName, domain, isReq(pair._2))
        else if (EncodeUtils.isList(pair._2) && !EncodeUtils.ofPrimitive(pair._2))
          ListOfNonPrimitiveProperty( item , propName, domain, isReq(pair._2))
        else if (EncodeUtils.isMap(pair._2) && EncodeUtils.ofPrimitive(pair._2))
          MapOfPrimitiveProperty( primitiveItem , propName, domain, isReq(pair._2))
        else if (EncodeUtils.isMap(pair._2) && !EncodeUtils.ofPrimitive(pair._2))
          MapOfNonPrimitiveProperty( item , propName, domain, isReq(pair._2))
        else if (EncodeUtils.isComplexType(pair._2))
          SubpropertyProperty(EncodeUtils.getLowerCaseStringField(pair._2, "Type"), propName, domain, isReq(pair._2))
        else EncodeUtils.getPrimitiveType(pair._2) match {
          case Some("string")     => StringProperty(propName, domain, isReq(pair._2))
          case Some("float")      => FloatProperty(propName, domain, isReq(pair._2))
          case Some("double")     => DoubleProperty(propName, domain, isReq(pair._2))
          case Some("long")       => LongProperty(propName, domain, isReq(pair._2))
          case Some("integer")    => IntProperty(propName, domain, isReq(pair._2))
          case Some("boolean")    => BooleanProperty(propName, domain, isReq(pair._2))
          case Some("timestamp")  => TimeStampProperty(propName, domain, isReq(pair._2))
          case Some("commadelimitedlist") => CommaDelimitedListProperty(propName, domain, isReq(pair._2))
          case Some("json")       => JsonProperty(propName, domain, isReq(pair._2))
          // Must be ONE malformed specification file, only happens for AWS::ServiceDiscovery::Instance instanceattributes property.
          case Some("map")        => MapOfPrimitiveProperty ( "string" , propName, domain, isReq(pair._2))
          case _               =>
            println("AWS::" + resSpecName + " found possibly malformed property declaration for " + propName)
            StringProperty(propName, domain, isReq(pair._2))
        }

      }
      else if (isInterResourceReference.isDefined)
      {
        val x = isInterResourceReference.get
        if ( x._4 )
          ResourceProperty(x._1,x._2, SpecificationRenaming.propertyName(domain.name,pair._1), domain, x._3)
        else
          ListOfResourcesProperty(x._1,x._2, SpecificationRenaming.propertyName(domain.name, pair._1), domain, x._3)

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

      val attrName = SpecificationRenaming.attributeName(domain.name,pair._1)

      if (EncodeUtils.isList(pair._2) && EncodeUtils.ofPrimitive(pair._2))
        ListOfPrimitiveAttribute(EncodeUtils.getLowerCaseStringField(pair._2, "PrimitiveItemType"), attrName, domain)
      else EncodeUtils.getPrimitiveType(pair._2) match {
        case Some("string")         => StringAttribute(attrName, domain)
        case Some("float")          => FloatAttribute(attrName, domain)
        case Some("double")         => DoubleAttribute(attrName, domain)
        case Some("long")           => LongAttribute(attrName, domain)
        case Some("integer")        => IntAttribute(attrName, domain)
        case Some("boolean")        => BooleanAttribute(attrName, domain)
        case Some("timestamp")      => TimeStamp(attrName, domain)
        case Some("commadelimitedlist") => CommaDelimitedListAttribute(attrName, domain)
        case Some("json")           => JsonAttribute(attrName, domain)
        case _                      => println("Function EncodeUtils.getPrimitive should not return this.")
          StringAttribute(attrName, domain)
      }
    }

    toListOf( EncodeUtils.getNodesAsMapOfJsons(json.field("Attributes").getOrElse(Json.jEmptyObject)) , toAttributeType )

  }





  private def toListOf [T <: GenericAttributeOrPropertyType] (nodesMap: Map[String,Json] , f: ((String,Json)) => T ) : Vector[T]  =
    nodesMap.toVector map f

  private def isReq(j:Json): Boolean = DecodeJson.BooleanDecodeJson.decodeJson( j.field("Required").getOrElse(
    Json.jBool(false)) ).toOption.get


}