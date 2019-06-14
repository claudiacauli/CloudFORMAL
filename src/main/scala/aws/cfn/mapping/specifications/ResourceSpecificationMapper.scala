package aws.cfn.mapping.specifications

import argonaut.Json
import aws.cfn.mapping.{CFnType, JsonUtils, Renaming, Specification}

import scala.language.postfixOps


private  object ResourceSpecificationMapper
{
   private[specifications]
   def fromJson(json: Json, resourceSpecificationName: String) : ResourceSpecification =
    new ResourceSpecificationMapper(json, resourceSpecificationName).encode()
}



private class ResourceSpecificationMapper
(json: Json, resourceSpecificationName: String)
{

  private val resSpecName: String = Renaming.ResSpecName(resourceSpecificationName)

  private def encode() = {

    val resourceTypeNode =
      JsonUtils.getNodesAsMapOfJsons(
        json.field(Specification.ResourceType).get).toVector
    val subpropertyTypeNodes =
      JsonUtils.getNodesAsMapOfJsons(
        json.field(Specification.PropertyTypes)
          .getOrElse(Json.jEmptyObject) ).toVector

    new ResourceSpecification(
      resSpecName,
      encodeResourceTypeWithName(resourceTypeNode.head._2, resourceTypeNode.head._1) ,
      subpropertyTypeNodes map (n => encodeSubpropertyTypeWithName(n._2, n._1))
    )

  }




  private def encodeResourceTypeWithName(j: Json, resourceType: String) = {
    val resource = ResourceValueType(Renaming.ResTypeName(resourceType))
    resource.properties = mapProperties(j, resource)
    resource.attributes = mapAttributes(j, resource)
    resource
  }


  private def encodeSubpropertyTypeWithName(j: Json, subpropType: String) = {
    val subProperty = SubpropertyValueType(Renaming.SubpTypeName(subpropType))
    subProperty.properties = mapProperties(j,subProperty)
    subProperty
  }


  private def mapProperties(j: Json, domain: NonPrimitiveValueType)= {
    toListOf(
      JsonUtils.getNodesAsMapOfJsons(
        j.field(Specification.Properties)
          .getOrElse(Json.jEmptyObject)) ,
      propertyMapper(j,domain))
  }


  private def mapAttributes(j:Json, domain: ResourceValueType)= {
    toListOf(
      JsonUtils.getNodesAsMapOfJsons(
        j.field(Specification.Attributes)
          .getOrElse(Json.jEmptyObject)),
      attributeMapper(j,domain))
  }


  private def toListOf[T<:GenericAttributeOrPropertyType]
  (nodesMap: Map[String,Json], mapperFunction: ((String,Json))=>T): Vector[T]  =
    nodesMap.toVector map mapperFunction


  private def propertyMapper(j: Json, domain: NonPrimitiveValueType)
  : ((String,Json)) => GenericPropertyType
  = pair =>
  {
    val propName              = Renaming.PropName(domain.name,pair._1)
    val fullPropName          = Renaming.UniquePropName( resSpecName, domain.name, pair._1)
    val isReferenceToResource = InterResourceReferencesMap.lookUp(fullPropName)
    val isPolicyDocument      = PolicyDocumentsReferencesMap.lookup(fullPropName)
    def primitiveItem   = JsonUtils.getLCStringField(pair._2,Specification.PrimitiveItemType)
    def complexType     = JsonUtils.getLCStringField(pair._2,Specification.Type)
    def item            = JsonUtils.getLCStringField(pair._2,Specification.ItemType)


    def getProperty: GenericPropertyType =
    {
      val isList      = JsonUtils.isList(pair._2)
      val isMap       = JsonUtils.isMap(pair._2)
      val isPrimitive = JsonUtils.ofPrimitive(pair._2)
      val isSubprop   = JsonUtils.isComplexType(pair._2)

      (isList,isMap,isPrimitive) match {
        case (true,_,true)  => ListOfPrimitiveProperty(primitiveItem,propName,domain,isReq(pair._2))
        case (true,_,false) => ListOfNonPrimitiveProperty(item,propName,domain,isReq(pair._2))
        case (_,true,true)  => MapOfPrimitiveProperty(primitiveItem,propName,domain,isReq(pair._2))
        case (_,true,false) => MapOfNonPrimitiveProperty(item ,propName,domain,isReq(pair._2))
        case _ =>
          if (isSubprop)
            SubpropertyProperty(complexType,propName,domain,isReq(pair._2))
          else
            JsonUtils.getPrimitiveType(pair._2) match {
              case Some(CFnType.String)     => StringProperty(propName,domain,isReq(pair._2))
              case Some(CFnType.Float)      => FloatProperty(propName,domain,isReq(pair._2))
              case Some(CFnType.Double)     => DoubleProperty(propName,domain,isReq(pair._2))
              case Some(CFnType.Long)       => LongProperty(propName,domain,isReq(pair._2))
              case Some(CFnType.Int)    => IntProperty(propName,domain,isReq(pair._2))
              case Some(CFnType.Bool)    => BooleanProperty(propName,domain,isReq(pair._2))
              case Some(CFnType.Timestamp)  => TimeStampProperty(propName,domain,isReq(pair._2))
              case Some(CFnType.Cdl)        => CommaDelimitedListProperty(propName,domain,isReq(pair._2))
              case Some(CFnType.Json)       => JsonProperty(propName,domain,isReq(pair._2))
              case Some(CFnType.Map)        => MapOfPrimitiveProperty (CFnType.String,propName,domain,isReq(pair._2))
              case _                        =>
                println("AWS::" + resSpecName + " found possibly malformed property declaration for " + propName)
                StringProperty(propName,domain,isReq(pair._2))
            }
      }
    }

    (isReferenceToResource,isPolicyDocument) match {
      //case (Some(_),Some(_))              => () // Cannot happen unless interRes and Policy keys intersect
      case (Some((s,r,req,fun)),None)     => getResource(s,r,req,fun,propName,domain)
      case (None,Some(_))                 => PolicyProperty(propName,domain,isReq(pair._2))
      case (None,None)                    => getProperty
    }
  }




  private def attributeMapper(j: Json, domain: ResourceValueType)
  : ((String,Json)) => GenericAttributeType
  = pair =>
  {
    val attrName = Renaming.AttrName(domain.name,pair._1)

    if (JsonUtils.isList(pair._2) && JsonUtils.ofPrimitive(pair._2))
      ListOfPrimitiveAttribute(
        JsonUtils.getLCStringField(pair._2, Specification.PrimitiveItemType),
        attrName,
        domain)
    else
      JsonUtils.getPrimitiveType(pair._2) match {
      case Some(CFnType.String)         => StringAttribute(attrName,domain)
      case Some(CFnType.Float)          => FloatAttribute(attrName,domain)
      case Some(CFnType.Double)         => DoubleAttribute(attrName,domain)
      case Some(CFnType.Long)           => LongAttribute(attrName,domain)
      case Some(CFnType.Int)        => IntAttribute(attrName,domain)
      case Some(CFnType.Bool)        => BooleanAttribute(attrName,domain)
      case Some(CFnType.Timestamp)      => TimeStamp(attrName,domain)
      case Some(CFnType.Cdl)            => CommaDelimitedListAttribute(attrName,domain)
      case Some(CFnType.Json)           => JsonAttribute(attrName,domain)
      case _                      => println("Function EncodeUtils.getPrimitive should not return this.")
        StringAttribute(attrName, domain)
    }
  }



  private def getResource(servRes: String, res: String, req: Boolean,
                          fun: Boolean, propName: String,
                          domain: NonPrimitiveValueType) =
    if (fun)
      ResourceProperty(servRes,res,propName,domain,req)
    else
      ListOfResourcesProperty(servRes,res,propName,domain,req)



  private def isReq(j:Json) =
    j.field(Specification.Required)
      .getOrElse(Json.jBool(false))
      .bool.getOrElse(false)


}
