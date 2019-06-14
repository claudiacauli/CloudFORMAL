package aws.cfn.mapping.templates

import argonaut.{DecodeJson, Json}
import aws.cfn.mapping.JsonUtils
import com.typesafe.scalalogging.StrictLogging

import scala.language.postfixOps


private class Json2TemplateEncoder(val ssE: Json2StackSetEncoder,
                                     templateName:String,
                                     templateJson:Json,
                                     templateDescriptor:Option[Json])
extends StrictLogging
{

  logger.debug(s"Initializing $templateName Template Encoder and Node Encoder.")

  private[templates] val template = new Template(templateName)
  private val NodeEncoder = new Json2NodeEncoder(tE = this)
  private val descriptors = getDescriptors

  logger.debug(s"Binding parameters, mappings, and condition for Template $templateName.")
  private[templates] val parameters       = getParametersMap
  private[templates] val mappings         = getMappings
  private[templates] val conditions       = getConditions
  private[templates] val resourceEncoders = getResourceEncoders
  logger.debug(s"Creating resources instances of Template $templateName.")
  private[templates] val resources        = createResources
  private[templates] val transforms       = getTransforms
  logger.debug(s"Binding Outputs for Template $templateName.")
  private[templates] val outputByLogicalId  = getOutputsByLogicalId
  private[templates] val outputByExportName = getOutputsMapByExportName
  private[templates] var policyEncoders: Vector[Json2PolicyDocumentEncoder] = Vector()


  def updateResourcesNames() : Unit = {
    resourceEncoders foreach (rE => {
      if (rE._2 == null) println ("Resource encoder has encoder null!")
      else rE._2.updateResourceName()
    })
  }


  def encode(): Template = {

    template.parameters         = this.parameters
    template.mappings           = this.mappings
    template.conditions         = this.conditions
    template.outputByLogicalId  = this.outputByLogicalId
    template.outputByExportName = this.outputByExportName
    template.resources = resources.toVector
      .flatMap (r =>
        Map(r._1 -> resourceEncoders(r._1).deepInstantiationOfResource())).toMap
    logger.debug(s"Instantiated properties for all resources of Template $templateName")
    template

  }




  private[templates]
  def hasTrueCondition(j: Json) = {

    def evaluateCondition(j: Json) =
      conditions.get( j.string.get.toLowerCase ) match {
        case None     => NodeEncoder.encode(j).asInstanceOf[BooleanNode].value
        case Some(b)  => b
      }

    !j.hasField(TemplateTag.Condition) ||
      evaluateCondition(j.field(TemplateTag.Condition).get)

  }




  private def getConditions : Map[String,Boolean] = {

    def evaluateCondition(c : (String,Json)) = {
      Map(c._1 -> NodeEncoder.encode(c._2).asInstanceOf[BooleanNode].value)
    }

    getSection(TemplateTag.Conditions).flatMap(evaluateCondition)
  }




  private def getParametersMap: Map[String,GenericValueNode] = {


    def parametersMapEntry (e: (String,Json)) : Map[String,GenericValueNode] =
      descriptors.get(e._1) match {
        case None =>
          e._2.field(TemplateTag.Default) match {
            case None => Map(e._1 -> NoValue)
            case Some(defaultJsonField) =>
              decodeJsonParameterValue(
                e._1, defaultJsonField, e._2.field(TemplateTag.Type)
                  .get.string.get.toLowerCase)
          }
        case Some(j) =>
          if (e._2.isObject)
            e._2.field(TemplateTag.Type) match {
              case None
                => decodeJsonParameterValue(e._1, j, ParameterType.String)
              case Some(t)
                => decodeJsonParameterValue(e._1, j, t.string.get.toLowerCase)
            }
          else
            decodeJsonParameterValue(e._1, j, ParameterType.String)
      }


    def decodeJsonParameterValue(pName:String, j: Json, pType: String) = {
      pType match {
        case ParameterType.String =>
          Map(pName -> StringNode( j match {
            case n if n.isString => n.string.get
            case n if n.isBool => n.bool.get.toString
            case n if n.isNumber => n.number.get.toString
          }))
        case ParameterType.Number =>
          Map(pName -> LongNode(j.number.get.truncateToLong))
        case ParameterType.ListOfNumber =>
          Map(pName -> ListNode(
            j.array.get
              .map( i => LongNode(i.number.get.truncateToLong)).toVector ))
        case ParameterType.CommaDelimitedList =>
          Map(pName -> CommaDelimitedListNode(j.string.get))
      }
    }



    (getSection(TemplateTag.Parameters) ++
      descriptors) flatMap parametersMapEntry

  }




  private def getOutputsMapByExportName: Map[String, Node] = {

    def outputByExportNameMapEntry(e: (String,Json)): Map[String,Node] = {
      if (hasTrueCondition(e._2))
        e._2.field(TemplateTag.Export) match {
          case None => Map()
          case Some(exportJson) => exportJson.field(TemplateTag.Name) match {
            case None => Map()
            case Some(exportNameJson) =>
              Map (
                NodeEncoder.encode(exportNameJson)
                  .asInstanceOf[StringNode].value.toLowerCase ->
                  NodeEncoder.encode(e._2.field(TemplateTag.ValueTag).get))
          }
        }
      else Map()
    }

    getSection(TemplateTag.Outputs)
      .flatMap(outputByExportNameMapEntry)
  }




  private def getOutputsByLogicalId : Map[String,Node] = {

    def outputByLogicalIdMapEntry(e: (String,Json)) : Map[String,Node] =
      if (hasTrueCondition(e._2))
        Map(e._1.toLowerCase -> NodeEncoder
          .encode(e._2.field(TemplateTag.ValueTag)
            .get))
      else
        Map()

    getSection(TemplateTag.Outputs)
      .flatMap(outputByLogicalIdMapEntry)
  }




  private def getSection(sectionName:String): Map[String, Json] =
    JsonUtils
      .getNodesAsMapOfJsons(
        templateJson.field(sectionName)
          .getOrElse(Json.jEmptyObject))



  private def getMappings: Map[String,Map[String,Either[String,Map[String,Any]]]] = {

    def getNodesAsMapOfStrings(json: Json): Either[String,Map[String,Any]] = {


      def subFieldToAnyObj(e : (String, Json)): (String,Any) =
        e._2 match {
          case sn if sn.isString  => (e._1, sn.string.get)
          case nn if nn.isNumber && nn.number.get.toInt.isDefined
          => (e._1, nn.number.get.toInt.get)
          case nn if nn.isNumber && nn.number.get.toFloat.isDefined
          => (e._1, nn.number.get.toFloat.get)
          case nn if nn.isNumber && nn.number.get.toDouble.isDefined
          => (e._1, nn.number.get.toDouble.get)
          case nn if nn.isNumber && nn.number.get.toLong.isDefined
          => (e._1, nn.number.get.toLong.get)
          case bn if bn.isBool    => (e._1, bn.bool.get)
          case an if an.isArray   => (e._1, an.array.get.toVector
            .map( n => subFieldToAnyObj((e._1,n))._2) )
          case _ => (e._1,DecodeJson.StringDecodeJson.decodeJson(e._2).toOption.get)
        }

      if (json.isObject)
        Right(
          JsonUtils.subFieldNames(json)
            .zip(JsonUtils.subFieldContents(json))
            .map (subFieldToAnyObj).toMap)
      else Left(json.string.get.toLowerCase)

    }

    getSection(TemplateTag.Mappings)
      .map(m =>
        (m._1 , JsonUtils.getNodesAsMapOfJsons(m._2).toVector
          .map (p =>
            (p._1 , getNodesAsMapOfStrings(p._2))).toMap
        )
      )
  }



  private def getDescriptors =
    JsonUtils
      .getNodesAsMapOfJsons(
        templateDescriptor
          .getOrElse(Json.jEmptyObject))



  private def getResourceEncoders =
    getSection(TemplateTag.Resources).toVector
      .map( e =>
        (e._1, new Json2ResourceEncoder(this,e._1,e._2))
      ).toMap



  private def createResources =
    resourceEncoders.toVector
      .flatMap(_._2.createResourceNodeWithAttributes).toMap



  private def getTransforms =
    getSection(TemplateTag.Transform)



}