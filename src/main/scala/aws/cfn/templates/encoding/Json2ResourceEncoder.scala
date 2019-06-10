package aws.cfn.templates.encoding

import java.io.File

import argonaut.{DecodeJson, Json}
import aws.cfn.dlmodel.{DLModelIRI, PrimitiveTypes}
import aws.cfn.templates.encoding.Json2ResourceEncoder.rangeNameOf
import aws.cfn.templates.formalization._
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.vocab.OWL2Datatype

import scala.jdk.OptionConverters._

object Json2ResourceEncoder {


  def subPropertiesNamesOfClassName (className: String, ssE: Json2StackSetEncoder,
                                     serviceType: String, resourceType: String,
                                     resourceOntology: OWLOntology): Set[String] = {


    def subPropertiesOfClassName  = {
      val classObj = Some(ssE.df.getOWLClass(DLModelIRI.resourceTypeIRI(serviceType+resourceType,className)))
      subPropertiesOf(classObj)
    }

    def allPropertiesInOntology =
      resourceOntology.dataPropertiesInSignature().toArray().toVector ++
        resourceOntology.objectPropertiesInSignature().toArray().toVector

    def propertyNameStartsWithClassName(p:AnyRef, className:String) =
      p match {
      case property: OWLObjectProperty
        => property.getIRI.toString.split("#").last.startsWith(className + "_")
      case _
        => p.asInstanceOf[OWLDataProperty].getIRI.toString.split("#").last.startsWith(className + "_")
    }

    def subPropertiesOf(classObj:Option[OWLClass]) = {
      classObj match {
        case None     => Vector()
        case Some(c)  =>
          val className = c.getIRI.toString.split("#").last
          (allPropertiesInOntology filter (p => propertyNameStartsWithClassName(p, className)))
            .map {
              case property: OWLDataProperty => Left(property)
              case p => Right(p.asInstanceOf[OWLObjectProperty])
            }
      }
    }




    (subPropertiesOfClassName map {
      case Left(dp)   => dp.getIRI.toString.split("#").last
      case Right(op)  => op.getIRI.toString.split("#").last
    }).toSet
  }




  def rangeNameOf(propertyName:String, resourceOntology: OWLOntology,
                  serviceType: String, resourceType: String,
                  ssE: Json2StackSetEncoder): Option[(String,String)] = {

    def rangeOf(propertyName:String): Option[Either[OWL2Datatype, OWLClass]] = {
      if (isObjectProperty(propertyName)) objectPropertyRange(propertyName) match {
        case None => None
        case Some(c) => Some(Right(c))
      }
      else if (isDataProperty(propertyName)) dataPropertyRange(propertyName) match {
        case None => None
        case Some(dt) => Some(Left(dt))
      }
      else None
    }

    def isObjectProperty(propertyName :String): Boolean  = {
      resourceOntology.containsObjectPropertyInSignature(
        DLModelIRI.propertyTypeIRI(serviceType+resourceType, propertyName ))
    }

    def isDataProperty( propertyName:String ): Boolean = {
      resourceOntology.containsDataPropertyInSignature(
        DLModelIRI.propertyTypeIRI(serviceType+resourceType, propertyName ))
    }

    def dataPropertyRange( propertyName:String ): Option[OWL2Datatype] = {
      resourceOntology.dataPropertyRangeAxioms(
        ssE.df.getOWLDataProperty( DLModelIRI.propertyTypeIRI(serviceType+resourceType,propertyName) ) )
        .findFirst().toScala match {
        case None => None
        case Some(ax) => Some(ax.datatypesInSignature().findFirst().get().getBuiltInDatatype)
      }
    }

    def objectPropertyRange ( propertyName:String ) : Option[OWLClass]  = {
      resourceOntology.objectPropertyRangeAxioms(
        resourceOntology.getOWLOntologyManager.getOWLDataFactory.getOWLObjectProperty( DLModelIRI.propertyTypeIRI(serviceType+resourceType,propertyName) ))
        .findFirst().toScala match {
        case None => None
        case Some(ax) =>
          Some( ax.classesInSignature().findFirst().get() )}
    }





    rangeOf(propertyName) match {
      case None => None
      case Some(Left(dt)) => Some(("",PrimitiveTypes.toString(dt)))
      case Some(Right(c)) => Some((c.getIRI.toString.split("#").head.split("/").last,
        c.getIRI.toString.split("#").last))
    }
  }


}






class Json2ResourceEncoder(iE: Json2InfrastructureEncoder, ssE: Json2StackSetEncoder, tE:Json2TemplateEncoder,
                                     resourceLogicalId:String, val resourceJsonNode:Json) {

  val serviceType: String = getServiceName
  val resourceType: String = getResourceType
  val NodeEncoder = new Json2NodeEncoder(iE, ssE,tE,this)
  val PolicyEncoder = new Json2PolicyDocumentEncoder(ssE,tE,this,NodeEncoder)
  var resource : StackSetResource = _





  def createResourceNodeWithAttributes: Map[String,StackSetResource] = {

    if (tE.hasTrueCondition(resourceJsonNode)){
      resource = StackSetResource( resourceLogicalId, serviceType, resourceType, attributesFromResourceJsonNode )
      importResourceSpecificationOntology()
      Map( resourceLogicalId -> resource )
    }
    else Map()

  }





  def deepInstantiationOfResource(): StackSetResource = {

    updateResourceByPolicy()

    resource.absentProperties =
      Json2ResourceEncoder.subPropertiesNamesOfClassName(resourceType.toLowerCase(),ssE,serviceType,resourceType,resourceOntology) --
      givenProperties.map(s=>s.toLowerCase())

    resource.givenProperties =
      (givenProperties flatMap (propName => Map(propName ->
      nodeObjectForProperty(propName, serviceType,resourceType, ssE, resourceOntology)))).toMap


    resource

  }





  def resourceOntology: OWLOntology = ssE.manager.getOntology(DLModelIRI.resourceTerminologyIRI(serviceType+resourceType))








  private def givenProperties: Set[String] =
    (resourceJsonNode.field("Properties").getOrElse(Json.jEmptyObject)
      .objectFieldsOrEmpty map (f => resourceType+"_"+f.toString)).toSet


  private def nodeObjectForProperty(propFullName: String, serviceType: String, resourceType:String,
                                    ssE :Json2StackSetEncoder, resourceOntology: OWLOntology) : Node = {

    val propTemplateName  = propFullName.split(resourceType+ "_").last
    val propContentNode   = resourceJsonNode.field("Properties").get.field(propTemplateName).get
    val propContentType   = rangeNameOf(propFullName, resourceOntology, serviceType, resourceType, ssE)

    NodeEncoder.encode(propContentNode,propContentType)

  }


  private def isCustomResource : Boolean =
    resourceJsonNode.field("Type").get.string.get == "AWS::CloudFormation::CustomResource" ||
    resourceJsonNode.field("Type").get.string.get.startsWith("Custom::")


  private def getServiceName: String =
    if ( isCustomResource )
      "cloudformation"
    else
      resourceJsonNode.field("Type").get.string.get.split("::")(1)


  private def getResourceType: String =
    if ( isCustomResource )
      "customresource"
    else
      resourceJsonNode.field("Type").get.string.get.split("::")(2)


  private def valueNodeFromSingleAttribute(attributeNode:Json) : GenericValueNode = {

    if (attributeNode.isString)
      StringNode( DecodeJson.StringDecodeJson.decodeJson(attributeNode).toOption.get )
    else if (attributeNode.isNumber)
      LongNode( DecodeJson.LongDecodeJson.decodeJson(attributeNode).toOption.get )
    else if (attributeNode.isBool)
      BooleanNode( DecodeJson.BooleanDecodeJson.decodeJson(attributeNode).toOption.get )
    else if (attributeNode.isArray)
      ListNode ( (attributeNode.array.get map valueNodeFromSingleAttribute).toVector )
    else NoValue

  }


  private def attributesFromResourceJsonNode : Map[String,GenericValueNode] = {
    resourceJsonNode.field("Attributes") match {
      case None => Map()
      case Some(attributesNode) =>
        (attributesNode.objectFieldsOrEmpty flatMap
          ( f => Map(f.toString.toLowerCase() -> valueNodeFromSingleAttribute(attributesNode.field(f).get)))
          ).toMap
    }
  }


  private def importResourceSpecificationOntology() : OWLOntology  = {
    ssE.manager.loadOntologyFromOntologyDocument(
      new File("src/main/resources/terminology/resourcespecificationsOwl/"
        + serviceType + resourceType + ".owl"))
  }



  def pointedResourceIsPolicy(res : Node) : Boolean = {
    if (res.isInstanceOf[StackSetResource])
      (res.asInstanceOf[StackSetResource].serviceType.toLowerCase, res.asInstanceOf[StackSetResource].resourceType.toLowerCase) match {
        case ("s3","bucketpolicy")    => true
        case ("iam","managedpolicy")  => true
        case ("iam","policy")         => true
        case ("iot","policy")         => true
        case ("sqs","queuepolicy")    => true
        case ("sns","topicpolicy")    => true
        case ("secretsmanager","resourcepolicy") => true
        case _ => false
      }
    else false
  }


  def currentResourceIsPolicyAndPointsTo : Set[StackSetResource] = {
    (resource.serviceType.toLowerCase,resource.resourceType.toLowerCase) match {
      case ("s3","bucketpolicy")    => {
        if (resourceJsonNode.field("Properties").get.field("Bucket").isDefined){
          NodeEncoder.encode(resourceJsonNode.field("Properties").get.field("Bucket").get) match {
            case r:StackSetResource => Set( r )
            case _ => Set()
          }
        }
        else Set()
      }
//      case ("iam","managedpolicy")  => true
//      case ("iam","policy")         => true
//      case ("iot","policy")         => true
//      case ("sqs","queuepolicy")    => true
//      case ("sns","topicpolicy")    => true
//      case ("secretsmanager","resourcepolicy") => true
        case _ => Set()
    }
  }

  def updateResourceByPolicy() = {
    currentResourceIsPolicyAndPointsTo foreach (pr => iE.updateResByPolicyMap(resource, pr))
  }

}
