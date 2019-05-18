package aws.cfn.encoding.template

import argonaut.{DecodeJson, Json}
import aws.cfn.encoding.EncodeUtils
import aws.cfn.formalization.{AndFunction, Arn, Base64Function, CidrFunction, EqualsFunction, FindInMapFunction, ForeignNode, GenericValueNode, GetAZsFunction, GetAttFunction, IfFunction, ImportValueFunction, JoinFunction, NoValue, Node, NotFunction, RefFunction, ResourceNode, SelectFunction, SplitFunction, StackSet, StackSetNode, SubFunction}

object JsonStackSetEncoder {

  // each template is (name, templateJson, descriptorJson)
  def encode(templates: Vector[(String,Json,Json)], stackSetName: String): StackSet = {
    new JsonStackSetEncoder(templates, stackSetName).encode()
  }

}



private class JsonStackSetEncoder(templates: Vector[(String,Json,Json)], stackSetName: String) {

  val stackSet = new StackSet(stackSetName)
  var arnsMap : Map[String,Either[ResourceNode,ForeignNode]] = Map()
  val outputs : (Map[String,Either[AnyVal,ResourceNode]], Map[String,Either[AnyVal,ResourceNode]]) = ???


  def encode(): StackSet = {

    for { t <- templates;  }

    // Create StackSet Object
    // Add to StackSet Object the outputsMap ( which you compute by going through all included templates outputs section )


    stackSet
  }


  def collectTemplatesOutputs() = {
    templates map templateOutputs

  }

  def templateOutputs(t: (String,Json,Json)) : (Map[String,Any], Map[String,Any]) =
    t._2.field("Outputs") match {
      case Some(os) => ( mapByEntry(os), mapByExportName(os) )
      case None => ( Map() , Map() )
    }

  def mapByEntry(json: Json) = json.objectFieldsOrEmpty flatMap (f => Map( f , encodeNode( json.field(f).get.field("Value").get)) )

  def mapEntriesFromSingleOutput ( j:Json ) =
    EncodeUtils.subFieldNames(j) zip EncodeUtils.subFieldContents(j)


  def outputValue (j : Json) = j.field("Value")



  def encodeTemplate(t : (String,Json,Json)) = {

    val mappings : Json = t._2.field("Mappings").get
    val resources : Json = t._2.field("Resources").get
    val parameters : Json = t._2.field("Parameters").get


    def encodeNode(json: Json): Any =
      if (json.isNull) NoValue
      else if (json.isObject) encodeObjectNode(json)
      else if (json.isArray)  encodeArrayNode(json)
      else encodeValueNode(json)


      def encodeArrayNode(json: Json) = json.array.get.toVector


      def encodeValueNode(json: Json) =
        if ( DecodeJson.StringDecodeJson.decodeJson(json).toOption.get.startsWith("arn:") )
          Arn(DecodeJson.StringDecodeJson.decodeJson(json).toOption.get, stackSet) ()
        else {

        }


      def encodeObjectNode(json:Json) {
          // Here it could be a Map or an Object or a Function
          val fieldNames = EncodeUtils.subFieldNames(json)
          fieldNames(0) match {
            case "Fn::Base64" => Base64Function(encodeNode(json.field("Fn::Base64").get).asInstanceOf[String])()
            case "Fn::Cidr" => CidrFunction(encodeNode(json.field("Fn::Cidr").get.array.get(0)).asInstanceOf[String],
              encodeNode(json.field("Fn::Cidr").get.array.get(1)).asInstanceOf[Int],
              encodeNode(json.field("Fn::Cidr").get.array.get(2)).asInstanceOf[Int])()
            case "Fn::If" => IfFunction(encodeNode(json.field("Fn::If").get.array.get(0)).asInstanceOf[Boolean],
              encodeNode(json.field("Fn::If").get.array.get(1)).asInstanceOf[GenericValueNode],
              encodeNode(json.field("Fn::If").get.array.get(2)).asInstanceOf[GenericValueNode])()
            case "Fn::Not" => NotFunction(encodeNode(json.field("Fn::Not").get).asInstanceOf[Boolean])()
            case "Fn::And" => AndFunction(encodeNode(json.field("Fn::And").get.array.get(0)).asInstanceOf[Boolean],
              encodeNode(json.field("Fn::And").get.array.get(1)).asInstanceOf[Boolean])()
            case "Fn::Equals" => EqualsFunction(encodeNode(json.field("Fn::Equals").get.array.get(0)).asInstanceOf[AnyVal],
              encodeNode(json.field("Fn::Equals").get.array.get(1)).asInstanceOf[AnyVal])()
            case "Fn::Or" => AndFunction(encodeNode(json.field("Fn::Or").get.array.get(0)).asInstanceOf[Boolean],
              encodeNode(json.field("Fn::Or").get.array.get(1)).asInstanceOf[Boolean])()
            case "Fn::FindInMap" =>
              if (json.field("Fn::FindInMap").get.array.get.size == 2) {
                FindInMapFunction(mappings,
                  encodeNode(json.field("Fn::FindInMap").get.array.get(0)).asInstanceOf[String],
                  encodeNode(json.field("Fn::FindInMap").get.array.get(1)).asInstanceOf[String])()
              } else {
                FindInMapFunction(mappings,
                  encodeNode(json.field("Fn::FindInMap").get.array.get(0)).asInstanceOf[String],
                  encodeNode(json.field("Fn::FindInMap").get.array.get(1)).asInstanceOf[String],
                  encodeNode(json.field("Fn::FindInMap").get.array.get(2)).asInstanceOf[String])()
              }
            case "Fn::GetAtt" => GetAttFunction(
              encodeNode (json.field("Fn::GetAtt").get.array.get(0)).asInstanceOf[String],
              encodeNode (json.field("Fn::GetAtt").get.array.get(1)).asInstanceOf[String], resources)()
            case "Fn::GetAZs" => GetAZsFunction ( encodeNode (json.field("Fn::GetAZs").get).asInstanceOf[String] )()
            case "Fn::ImportValue" => ImportValueFunction ( encodeNode(json.field("Fn::ImportValue").get).asInstanceOf[String], outputs)()
            case "Fn::Join" => JoinFunction (
                encodeNode( json.field("Fn::Join").get.array.get(0)).asInstanceOf[String],
                encodeNode( json.field("Fn::Join").get.array.get(1)).asInstanceOf[Vector[String]] )()
            case "Fn::Select" => SelectFunction (
                encodeNode(json.field("Fn::Select").get.array.get(0)).asInstanceOf[Int],
                encodeNode(json.field("Fn::Select").get.array.get(1)).asInstanceOf[Vector[AnyVal]] )()
            case "Fn::Split" => SplitFunction (
              encodeNode(json.field("Fn::Split").get.array.get(0)).asInstanceOf[String],
              encodeNode(json.field("Fn::Split").get.array.get(1)).asInstanceOf[String] ) ()
            case "Fn::Sub" =>
              if (json.field("Fn::Sub").get.array.get.size == 2) {
                SubFunction ( resources, parameters, encodeNode(json.field("Fn::Sub").get.array.get(0)).asInstanceOf[String],
                Some(EncodeUtils.getNodesAsMapOfStrings( json.field("Fn::Sub").get.array.get(1) )) ) ()
              }
              else
                SubFunction ( resources, parameters, encodeNode(json.field("Fn::Sub").get.array.get(0)).asInstanceOf[String] ) ()
            case "Fn::Transform" => json.toString()   // TODO!
            case "Ref" => RefFunction(
              encodeNode( json.field("Ref").get ).asInstanceOf[String],
              resources, parameters )()
            case _ => json
          }
      }


  }


}
