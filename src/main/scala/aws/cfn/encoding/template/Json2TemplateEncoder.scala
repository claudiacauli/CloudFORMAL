package aws.cfn.encoding.template

import argonaut.{DecodeJson, Json}
import aws.cfn.encoding.EncodeUtils
import aws.cfn.encoding.EncodeUtils.{subFieldContents, subFieldNames}
import aws.cfn.formalization._
import scala.language.postfixOps


protected class Json2TemplateEncoder(ssE: Json2StackSetEncoder, templateName:String, templateJson:Json, templateDescriptor:Option[Json]){


  val template = new Template(templateName)
  val NodeEncoder: Json2NodeEncoder = new Json2NodeEncoder(ssE, this, null) // TODO
  val templateDescriptorAsMapOfJsons : Map[String,Json] = EncodeUtils.getNodesAsMapOfJsons(templateDescriptor.getOrElse(Json.jEmptyObject))
  val templateTransformAsMapOfJson: Map[String, Json] = getSection("Transform") // TODO!
  val parameters: Map[String,Node] = getParametersMap
  val mappings: Map[String,Map[String,Either[String,Map[String,String]]]] = getMappings()//getSection("Mappings")
  val conditions: Map[String,Boolean] = getConditions
  val resourceEncoders : Map[String,Json2ResourceEncoder] = (getSection("Resources").toVector map ( e => (e._1, new Json2ResourceEncoder(ssE,this,e._1,e._2)))).toMap
  val resources: Map[String,ResourceNode] = (resourceEncoders.toVector flatMap ( rE => rE._2.createResourceNodeWithAttributes )).toMap
  val outputByLogicalId: Map[String, Node] = getOutputsByLogicalId
  val outputByExportName: Map[String, Node] = getOutputsMapByExportName
  val resourceByArn: Map[String,Node] = createResourceByArnMap




  def encode(): Template = {
    template.resources = (resources.toVector flatMap (r => Map(r._1 -> resourceEncoders(r._1).deepInstantiationOfResource()))).toMap

    // For each resource, do a deep instantiation
    // Resource Node with name and attributes already exists.
    // Now you need to go through all possible properties from the OWL Spec file and either put them in the present
    // or in the absent list of properties
    // the absent ones you only need their name
    // the present ones are a map of Name,Node (this is because they can point to foreign nodes, resource nodes, subprop nodes etc.
    // Fetch from Ontology with res type name the list of properties that have domain the current node
    // go through all of them
    // put them either in the absent or present
    // for the present, get the node corresponding to their subproperty node

    template
  }



  private def createResourceByArnMap: Map[String, ResourceNode] = {

    def arnResourcePairFromResources( e: (String, ResourceNode)) : Map[String,ResourceNode] = {
      if (e._2.attributes.contains("Arn"))
        Map ( e._2.attributes("Arn").asInstanceOf[StringNode].value -> e._2 )
      else Map()
    }

    (resources.toList flatMap arnResourcePairFromResources).toMap
  }


  def isConditionTrue(json : Json) : Boolean = {

    def evaluateCondition(json: Json) : Boolean = {
      conditions.get( DecodeJson.StringDecodeJson.decodeJson(json).toOption.get ) match {
        case None => NodeEncoder.encode(json).asInstanceOf[BooleanNode].value
        case Some(b) => b
      }
    }

    !json.hasField("Condition") || evaluateCondition(json.field("Condition").get)
  }


  private def getConditions : Map[String,Boolean] = {

    def evaluateCondition(c : (String,Json)) : Map[String,Boolean] = {
      Map(c._1 -> NodeEncoder.encode(c._2).asInstanceOf[BooleanNode].value)
    }

    getSection("Conditions") flatMap evaluateCondition
  }


  private def getParametersMap : Map[String,Node] = {

    def parametersMapEntry (e: (String,Json)) : Map[String,Node] = {

      templateDescriptorAsMapOfJsons.get(e._1) match {
        case None =>
          e._2.field("Default") match {
            case None => Map(e._1 -> NoValue)
            case Some(defaultJsonField) =>
              decodeJsonParameterValue(e._1, defaultJsonField, DecodeJson.StringDecodeJson.decodeJson(e._2.field("Type").get).toOption.get)
          }
        case Some(jsonValue) => {
          if (e._2.isObject)
            e._2.field("Type") match {
              case None =>  decodeJsonParameterValue(e._1, jsonValue, "String")
              case Some(t) => decodeJsonParameterValue(e._1, jsonValue, DecodeJson.StringDecodeJson.decodeJson(e._2.field("Type").get).toOption.get)
            }
          else decodeJsonParameterValue(e._1, jsonValue, "String")
        }
      }
    }


    def decodeJsonParameterValue(paramName:String, jsonValue: Json, paramType:String): Map[String,Node] = paramType match {
      case "String" =>
        Map(paramName -> StringNode(DecodeJson.StringDecodeJson.decodeJson(jsonValue).toOption.get))
      case "Number" =>
        Map(paramName -> LongNode(DecodeJson.IntDecodeJson.decodeJson(jsonValue).toOption.get))
      case "List<Number>" =>
        Map(paramName -> ListNode( (jsonValue.array.get map ( i => LongNode(DecodeJson.IntDecodeJson.decodeJson(i).toOption.get ))).toVector ))
      case "CommaDelimitedList" =>
        Map(paramName -> CommaDelimitedListNode( DecodeJson.StringDecodeJson.decodeJson(jsonValue).toOption.get))
    }

    (getSection("Parameters") ++
      templateDescriptorAsMapOfJsons) flatMap parametersMapEntry

  }


  private def getOutputsMapByExportName: Map[String, Node] = {

    def outputByExportNameMapEntry(e: (String,Json)): Map[String,Node] = {
      if ( isConditionTrue(e._2) )
        e._2.field("Export") match {
          case None => Map()
          case Some(exportJson) => exportJson.field("Name") match {
            case None => Map()
            case Some(exportNameJson) =>
              Map ( NodeEncoder.encode(exportNameJson).asInstanceOf[StringNode].value -> NodeEncoder.encode(e._2.field("Value").get))
          }
        }
      else Map()
    }

    getSection("Outputs") flatMap outputByExportNameMapEntry
  }


  private def getOutputsByLogicalId : Map[String,Node] = {

    def outputByLogicalIdMapEntry(e: (String,Json)) : Map[String,Node] = {
      if (isConditionTrue(e._2))
        Map(e._1 -> NodeEncoder.encode(e._2.field("Value").get))
      else
        Map()
    }

    getSection("Outputs") flatMap outputByLogicalIdMapEntry
  }


  private def getSection(sectionName:String): Map[String, Json] = {
    EncodeUtils.getNodesAsMapOfJsons( templateJson.field(sectionName).getOrElse(Json.jEmptyObject) )
  }

  private def getMappings() : Map[String,Map[String,Either[String,Map[String,String]]]] = {

    def getNodesAsMapOfStrings(json: Json) =
      if (json.isObject)
        Right(subFieldNames(json) zip (subFieldContents(json) map (
      c => DecodeJson.StringDecodeJson.decodeJson(c).toOption.get) ) toMap)
      else Left(DecodeJson.StringDecodeJson.decodeJson(json).toOption.get.toLowerCase)


    getSection("Mappings") map (m => (m._1 ,
      ( EncodeUtils.getNodesAsMapOfJsons(m._2).toVector map ( p => (p._1 , getNodesAsMapOfStrings(p._2))) ).toMap
    ))
  }

}