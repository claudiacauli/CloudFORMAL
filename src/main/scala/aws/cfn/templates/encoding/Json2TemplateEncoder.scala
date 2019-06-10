package aws.cfn.templates.encoding

import argonaut.{DecodeJson, Json}
import aws.cfn.shared.EncodeUtils
import aws.cfn.shared.EncodeUtils.{subFieldContents, subFieldNames}
import aws.cfn.templates.formalization._

import scala.language.postfixOps


protected class Json2TemplateEncoder(iE:Json2InfrastructureEncoder, ssE: Json2StackSetEncoder, templateName:String, templateJson:Json, templateDescriptor:Option[Json]){

  val template = new Template(templateName)
  val NodeEncoder: Json2NodeEncoder = new Json2NodeEncoder(iE, ssE, this, null)
  val templateDescriptorAsMapOfJsons : Map[String,Json] = EncodeUtils.getNodesAsMapOfJsons(templateDescriptor.getOrElse(Json.jEmptyObject))
  val templateTransformAsMapOfJson: Map[String, Json] = getSection("Transform") // TODO!
  val parameters: Map[String,GenericValueNode] = getParametersMap
  val mappings: Map[String,Map[String,Either[String,Map[String,Any]]]] = getMappings//getSection("Mappings")
  val conditions: Map[String,Boolean] = getConditions
  val resourceEncoders : Map[String,Json2ResourceEncoder] = (getSection("Resources").toVector map ( e => (e._1, new Json2ResourceEncoder(iE,ssE,this,e._1,e._2)))).toMap
  val resources: Map[String,StackSetResource] = (resourceEncoders.toVector flatMap (rE => rE._2.createResourceNodeWithAttributes )).toMap
  var embeddedPolicies : Map[String,StackSetResource] = Map()
  val outputByLogicalId: Map[String, Node] = getOutputsByLogicalId
  val outputByExportName: Map[String, Node] = getOutputsMapByExportName
  var resourceByArn: Map[String,Node] = Map()//createResourceByArnMap




  def encode(): Template = {
    template.resources = (resources.toVector flatMap (r => Map(r._1 -> resourceEncoders(r._1).deepInstantiationOfResource()))).toMap
    template.resources = template.resources ++ embeddedPolicies
    template
  }


  override def toString: String = {
    "\t\tTemplate: " + templateName + "\n" +
    prettyString(parameters,"Parameters") +
    prettyString(mappings, "Mappings") +
    prettyString(conditions, "Conditions") +
    prettyString(outputByLogicalId, "Outputs By ID") +
    prettyString(outputByExportName, "Outputs By Export Name")
  }




  private def createResourceByArnMap: Map[String, StackSetResource] = {

    def arnResourcePairFromResources( e: (String, StackSetResource)) : Map[String,StackSetResource] = {
      if (e._2.attributes.contains("Arn"))
        Map ( e._2.attributes("Arn").asInstanceOf[StringNode].value -> e._2 )
      else Map()
    }

    (resources.toList flatMap arnResourcePairFromResources).toMap
  }


  def hasTrueCondition(json : Json) : Boolean = {

    def evaluateCondition(json: Json) : Boolean = {
      conditions.get( DecodeJson.StringDecodeJson.decodeJson(json).toOption.get.toLowerCase() ) match {
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


  private def getParametersMap : Map[String,GenericValueNode] = {

    def parametersMapEntry (e: (String,Json)) : Map[String,GenericValueNode] = {

      templateDescriptorAsMapOfJsons.get(e._1) match {
        case None =>
          e._2.field("Default") match {
            case None => Map(e._1 -> NoValue)
            case Some(defaultJsonField) =>
              decodeJsonParameterValue(e._1, defaultJsonField, DecodeJson.StringDecodeJson.decodeJson(e._2.field("Type").get).toOption.get)
          }
        case Some(jsonValue) =>
          if (e._2.isObject)
            e._2.field("Type") match {
              case None =>  decodeJsonParameterValue(e._1, jsonValue, "String")
              case Some(t) => decodeJsonParameterValue(e._1, jsonValue, t.string.get)
            }
          else decodeJsonParameterValue(e._1, jsonValue, "String")
      }
    }


    def decodeJsonParameterValue(paramName:String, jsonValue: Json, paramType:String): Map[String,GenericValueNode] = paramType match {
      case "String" =>
        Map(paramName -> StringNode(DecodeJson.StringDecodeJson.decodeJson(jsonValue).toOption.get.toLowerCase()))
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
      if ( hasTrueCondition(e._2) )
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
      if (hasTrueCondition(e._2))
        Map(e._1 -> NodeEncoder.encode(e._2.field("Value").get))
      else
        Map()
    }

    getSection("Outputs") flatMap outputByLogicalIdMapEntry
  }


  private def getSection(sectionName:String): Map[String, Json] = {
    EncodeUtils.getNodesAsMapOfJsons( templateJson.field(sectionName).getOrElse(Json.jEmptyObject) )
  }

  private def getMappings: Map[String,Map[String,Either[String,Map[String,Any]]]] = {

    def getNodesAsMapOfStrings(json: Json): Either[String,Map[String,Any]] = {

      def subFieldToAnyObj(e : (String, Json)): (String,Any) =  {
        e._2 match {
          case sn if sn.isString => (e._1, DecodeJson.StringDecodeJson.decodeJson(sn).toOption.get )
          case nn if nn.isNumber => (e._1,DecodeJson.FloatDecodeJson.decodeJson(nn).toOption.get)
          case bn if bn.isBool => (e._1,DecodeJson.BooleanDecodeJson.decodeJson(bn).toOption.get)
          case an if an.isArray => (e._1, an.array.get.toVector map ( n => subFieldToAnyObj((e._1,n))._2) )
          case _ => (e._1,DecodeJson.StringDecodeJson.decodeJson(e._2).toOption.get)
        }
      }

      if (json.isObject)
        Right( ((subFieldNames(json) zip subFieldContents(json)) map subFieldToAnyObj).toMap )
      else Left(DecodeJson.StringDecodeJson.decodeJson(json).toOption.get.toLowerCase)

    }

    getSection("Mappings") map (m => (m._1 ,
      ( EncodeUtils.getNodesAsMapOfJsons(m._2).toVector map ( p => (p._1 , getNodesAsMapOfStrings(p._2))) ).toMap
    ))
  }



  private def prettyString(map : Map[String,Any], s : String) = {
    if (map.nonEmpty)
      "\t\t "+s+": \n"   + map.foldLeft("")((a,b)=> a + "\t\t\t("+b._1+" -> "+b._2+")\n")
    else ""
  }

}