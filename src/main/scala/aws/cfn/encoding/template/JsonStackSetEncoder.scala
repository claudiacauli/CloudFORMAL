package aws.cfn.encoding.template

import argonaut.{DecodeJson, EncodeJson, Json}
import aws.cfn.encoding.EncodeUtils
import aws.cfn.formalization.{AndFunction, Arn, Base64Function, CidrFunction, EqualsFunction, FindInMapFunction, ForeignNode, GenericValueNode, GetAZsFunction, GetAttFunction, IfFunction, ImportValueFunction, JoinFunction, NoValue, Node, NotFunction, RefFunction, ResourceNode, SelectFunction, SplitFunction, StackSet, StackSetNode, SubFunction, Template}

object JsonStackSetEncoder {

  // each template is (name, templateJson, descriptorJson)
  def encode(templates: Vector[(String,Json,Json)], stackSetName: String): StackSet = {
    new JsonStackSetEncoder(templates, stackSetName).encode()
  }

}



private class JsonStackSetEncoder(templates: Vector[(String,Json,Json)], stackSetName: String) {

  val stackSet = new StackSet(stackSetName)
  //var arnsMap : Map[String,Either[ResourceNode,ForeignNode]] = Map()
  var outputsByExportName : Map[String,Any] = Map()


  def encode(): StackSet = {

    val templatesEncoders: Vector[JsonTemplateEncoder] = templates map (t => new JsonTemplateEncoder(this,stackSet,t._1,t._2, t._3))
    outputsByExportName = (templatesEncoders flatMap ( te => te.outputsByExportName )).toMap

    stackSet
  }




}

private class JsonTemplateEncoder(ssE: JsonStackSetEncoder, ss: StackSet, templateName:String, templateJson:Json, templateDescriptor:Json){

  val template = new Template(templateName)
  val NodeEncoder: JsonStackSetNodeEncoder = new JsonStackSetNodeEncoder(ssE, ss, this, template)
  val parameters: Map[String,Any] = getParameters
  val mappings: Map[String,Json] = templateMappingsAsMapOfJsons
  val conditions: Map[String,Boolean] = getConditions
  val outputsByLogicalId: Map[String, Any] = getOutputsByLogicalId
  val outputsByExportName: Map[String, Any] = getOutputsMapByExportName

  val templateDescriptorAsMapOfJsons : Map[String,Json] = EncodeUtils.getNodesAsMapOfJsons(templateDescriptor)
  val templateParametersAsMapOfJsons: Map[String, Json] = getSection("Parameters")
  val templateMappingsAsMapOfJsons: Map[String, Json] = getSection("Mappings")
  val templateConditionsAsMapOfJsons: Map[String, Json] = getSection("Conditions")
  val templateTransformAsMapOfJson: Map[String, Json] = getSection("Transform")
  val templateResourcesAsMapOfJson: Map[String, Json] = getSection("Resources")


  def encode(): Template = {


    template
  }



  private def getParameters : Map[String,Any] = {

    def parametersMapEntry (e: (String,Json)) : Map[String,Any] = templateDescriptorAsMapOfJsons.get(e._1) match {
      case None => e._2.field("Default") match {
        case None => Map(e._1 -> "")
        case Some(defaultJsonField) =>
          decodeJsonParameterValue(e._1, defaultJsonField, DecodeJson.StringDecodeJson.decodeJson(e._2.field("Type").get).toOption.get)
      }
      case Some(jsonValue) =>
        decodeJsonParameterValue(e._1, jsonValue, DecodeJson.StringDecodeJson.decodeJson(e._2.field("Type").get).toOption.get)
    }

    def decodeJsonParameterValue(paramName:String, jsonValue: Json, paramType:String): Map[String,Any] = paramType match {
      case "String" => Map(paramName -> DecodeJson.StringDecodeJson.decodeJson(jsonValue).toOption.get)
      case "Number" => Map(paramName -> DecodeJson.IntDecodeJson.decodeJson(jsonValue).toOption.get)
      case "List<Number>" => Map(paramName -> (jsonValue.array.get map ( i => DecodeJson.IntDecodeJson.decodeJson(i).toOption.get)).toVector )
      case "CommaDelimitedList" => Map(paramName -> DecodeJson.StringDecodeJson.decodeJson(jsonValue).toOption.get.split(",").toVector)
    }

    (templateParametersAsMapOfJsons ++ templateDescriptorAsMapOfJsons) flatMap parametersMapEntry

  }






  private def getSection(sectionName:String): Map[String, Json] =
    EncodeUtils.getNodesAsMapOfJsons( templateJson.field(sectionName).getOrElse(Json.jEmptyObject) )

  private def getOutputsMapByExportName: Map[String, Any] = getSection("Outputs") flatMap outputByExportNameMapEntry
  private def outputByExportNameMapEntry(e: (String,Json)): Map[String,Any] = e._2.field("Export") match {
    case None => Map()
    case Some(exportJson) => exportJson.field("Name") match {
      case None => Map()
      case Some(exportNameJson) => Map ( NodeEncoder.encode(exportNameJson).asInstanceOf[String] -> NodeEncoder.encode(e._2.field("Value").get))
    }
  }

  private def getOutputsByLogicalId : Map[String,Any] = getSection("Outputs") flatMap outputByLogicalIdMapEntry
  private def outputByLogicalIdMapEntry(e: (String,Json)) : Map[String,Any]= Map(e._1 -> NodeEncoder.encode(e._2.field("Value").get))

}













private class JsonStackSetNodeEncoder(ssE: JsonStackSetEncoder, ss:StackSet, tE: JsonTemplateEncoder, t:Template){

  def encode(json: Json): Any =
    if (json.isNull) NoValue
    else if (json.isObject) encodeObjectNode(json)
    else if (json.isArray)  encodeArrayNode(json)
    else encodeValueNode(json)


  def encodeArrayNode(json: Json) = json.array.get.toVector


  def encodeValueNode(json: Json) =
    if ( DecodeJson.StringDecodeJson.decodeJson(json).toOption.get.startsWith("arn:") )
      Arn(DecodeJson.StringDecodeJson.decodeJson(json).toOption.get, ss) ()
    else {

    }


  def encodeObjectNode(json:Json) {
    // Here it could be a Map or an Object or a Function
    val fieldNames = EncodeUtils.subFieldNames(json)
    fieldNames(0) match {
      case "Fn::Base64" => Base64Function(encode(json.field("Fn::Base64").get).asInstanceOf[String])()
      case "Fn::Cidr" => CidrFunction(encode(json.field("Fn::Cidr").get.array.get(0)).asInstanceOf[String],
        encode(json.field("Fn::Cidr").get.array.get(1)).asInstanceOf[Int],
        encode(json.field("Fn::Cidr").get.array.get(2)).asInstanceOf[Int])()
      case "Fn::If" => IfFunction(encode(json.field("Fn::If").get.array.get(0)).asInstanceOf[Boolean],
        encode(json.field("Fn::If").get.array.get(1)).asInstanceOf[GenericValueNode],
        encode(json.field("Fn::If").get.array.get(2)).asInstanceOf[GenericValueNode])()
      case "Fn::Not" => NotFunction(encode(json.field("Fn::Not").get).asInstanceOf[Boolean])()
      case "Fn::And" => AndFunction(encode(json.field("Fn::And").get.array.get(0)).asInstanceOf[Boolean],
        encode(json.field("Fn::And").get.array.get(1)).asInstanceOf[Boolean])()
      case "Fn::Equals" => EqualsFunction(encode(json.field("Fn::Equals").get.array.get(0)).asInstanceOf[AnyVal],
        encode(json.field("Fn::Equals").get.array.get(1)).asInstanceOf[AnyVal])()
      case "Fn::Or" => AndFunction(encode(json.field("Fn::Or").get.array.get(0)).asInstanceOf[Boolean],
        encode(json.field("Fn::Or").get.array.get(1)).asInstanceOf[Boolean])()
      case "Fn::FindInMap" =>
        if (json.field("Fn::FindInMap").get.array.get.size == 2) {
          FindInMapFunction(tE.mappings,
            encode(json.field("Fn::FindInMap").get.array.get(0)).asInstanceOf[String],
            encode(json.field("Fn::FindInMap").get.array.get(1)).asInstanceOf[String])()
        } else {
          FindInMapFunction(tE.mappings,
            encode(json.field("Fn::FindInMap").get.array.get(0)).asInstanceOf[String],
            encode(json.field("Fn::FindInMap").get.array.get(1)).asInstanceOf[String],
            encode(json.field("Fn::FindInMap").get.array.get(2)).asInstanceOf[String])()
        }
      case "Fn::GetAtt" => GetAttFunction(
        encode (json.field("Fn::GetAtt").get.array.get(0)).asInstanceOf[String],
        encode (json.field("Fn::GetAtt").get.array.get(1)).asInstanceOf[String], resources)()
      case "Fn::GetAZs" => GetAZsFunction ( encode (json.field("Fn::GetAZs").get).asInstanceOf[String] )()
      case "Fn::ImportValue" =>
        ImportValueFunction ( encode(json.field("Fn::ImportValue").get).asInstanceOf[String],
          ssE.outputsByExportName, tE.outputsByLogicalId)()
      case "Fn::Join" => JoinFunction (
        encode( json.field("Fn::Join").get.array.get(0)).asInstanceOf[String],
        encode( json.field("Fn::Join").get.array.get(1)).asInstanceOf[Vector[String]] )()
      case "Fn::Select" => SelectFunction (
        encode(json.field("Fn::Select").get.array.get(0)).asInstanceOf[Int],
        encode(json.field("Fn::Select").get.array.get(1)).asInstanceOf[Vector[AnyVal]] )()
      case "Fn::Split" => SplitFunction (
        encode(json.field("Fn::Split").get.array.get(0)).asInstanceOf[String],
        encode(json.field("Fn::Split").get.array.get(1)).asInstanceOf[String] ) ()
      case "Fn::Sub" =>
        if (json.field("Fn::Sub").get.array.get.size == 2) {
          SubFunction ( resources, tE.parameters, encode(json.field("Fn::Sub").get.array.get(0)).asInstanceOf[String],
            Some(EncodeUtils.getNodesAsMapOfStrings( json.field("Fn::Sub").get.array.get(1) )) ) ()
        }
        else
          SubFunction ( resources, tE.parameters, encode(json.field("Fn::Sub").get.array.get(0)).asInstanceOf[String] ) ()
      case "Fn::Transform" => json.toString()   // TODO!
      case "Ref" => RefFunction(
        encode( json.field("Ref").get ).asInstanceOf[String],
        resources, tE.parameters )()
      case _ => json
    }
  }




}