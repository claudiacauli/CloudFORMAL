package aws.cfn.templates.encoding

import java.io.File

import argonaut.{DecodeJson, Json}
import aws.cfn.dlmodel.{PrimitiveTypes, DLModelIRI}
import aws.cfn.templates.formalization._
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.vocab.OWL2Datatype

import scala.jdk.OptionConverters._

protected class Json2ResourceEncoder(ssE: Json2StackSetEncoder, tE:Json2TemplateEncoder, resourceLogicalId:String, resourceJsonNode:Json) {

  val serviceType: String = getServiceName
  val resourceType: String = getResourceType
  val NodeEncoder = new Json2NodeEncoder(ssE,tE,this)
  val PolicyEncoder = new Json2PolicyEncoder(ssE,tE,this,NodeEncoder)
  var resource : StackSetResource = null


  def createResourceNodeWithAttributes: Map[String,StackSetResource] = {

    if (tE.hasTrueCondition(resourceJsonNode)){
      resource = StackSetResource( resourceLogicalId, serviceType, resourceType, attributesFromResourceJsonNode )
      importResourceSpecificationOntology
      Map( resourceLogicalId -> resource )
    }
    else Map()

  }






  def deepInstantiationOfResource(): StackSetResource = {

    resource.absentProperties = subPropertiesNamesOfClassName(resourceType.toLowerCase()) -- givenProperties().map(s=>s.toLowerCase())
    resource.givenProperties = (givenProperties() flatMap (propName => Map(propName -> nodeObjectForProperty(propName)))).toMap
    resource

  }

  def nodeObjectForProperty(propFullName: String) : Node = {
    val propTemplateName = propFullName.split(resourceType+ "_").last
    NodeEncoder.encode( resourceJsonNode.field("Properties").get.field(propTemplateName).get, rangeNameOf(propFullName))
  }

  def givenProperties(): Set[String] =
    (resourceJsonNode.field("Properties").getOrElse(Json.jEmptyObject).objectFieldsOrEmpty map (f => resourceType+"_"+f.toString)).toSet


  def isCustomResource : Boolean =
    DecodeJson.StringDecodeJson.decodeJson(resourceJsonNode.field("Type").get).toOption.get.equals("AWS::CloudFormation::CustomResource") ||
      DecodeJson.StringDecodeJson.decodeJson(resourceJsonNode.field("Type").get).toOption.get.startsWith("Custom::")


  def getServiceName: String =
      if ( isCustomResource ) "cloudformation"
      else DecodeJson.StringDecodeJson.decodeJson( resourceJsonNode.field("Type").get ).toOption.get.split("::")(1)

  def getResourceType: String =
    if ( isCustomResource ) "customresource"
    else DecodeJson.StringDecodeJson.decodeJson( resourceJsonNode.field("Type").get ).toOption.get.split("::")(2)


  def subPropertiesNamesOfClassName (conceptName:String): Set[String] = {
    (subPropertiesOfClassName(conceptName) map ( p => 
      p match {
        case Left(dp) => dp.getIRI.toString.split("#").last
        case Right(op) => op.getIRI.toString.split("#").last
      } )).toSet
  }

  def subPropertiesOfClassName ( conceptName : String ): Vector[Either[OWLDataProperty,OWLObjectProperty]]  = {
    subPropertiesOf(Some(ssE.manager.getOWLDataFactory.getOWLClass(DLModelIRI.resourceTypeIRI(serviceType+resourceType, conceptName))))
  }


  def subPropertiesOf ( concept : Option[OWLClass]  ): Vector[Either[OWLDataProperty, OWLObjectProperty]] = concept match {
    case None  => Vector()
    case Some(c) => {
      val className = c.getIRI.toString.split("#").last

      (resourceOntology.dataPropertiesInSignature().toArray().toVector
        ++ resourceOntology.objectPropertiesInSignature().toArray().toVector).filter(
        p => p match {
          case property: OWLObjectProperty => property.getIRI.toString.split("#").last.startsWith(className + "_")
          case _ => p.asInstanceOf[OWLDataProperty].getIRI.toString.split("#").last.startsWith(className + "_")
        }
      ).map( p =>
        p match {
          case property: OWLDataProperty => Left(property)
          case _ => Right(p.asInstanceOf[OWLObjectProperty])
        }
      )

    }
  }

  def rangeNameOf(propertyName:String): Option[String] = {
    rangeOf(propertyName) match {
      case None => None
      case Some(Left(dt)) => Some(PrimitiveTypes.toString(dt))
      case Some(Right(c)) => Some(c.getIRI.toString.split("#").last)
    }
  }


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


  def isObjectProperty( propertyName : String ): Boolean  = {
    resourceOntology.containsObjectPropertyInSignature(DLModelIRI.propertyTypeIRI(serviceType+resourceType, propertyName))
  }

  def isDataProperty( propertyName:String ): Boolean = {
    resourceOntology.containsDataPropertyInSignature(DLModelIRI.propertyTypeIRI(serviceType+resourceType, propertyName))
  }

  def dataPropertyRange( propertyName:String ): Option[OWL2Datatype] = {
    resourceOntology.dataPropertyRangeAxioms(
      ssE.manager.getOWLDataFactory.getOWLDataProperty( DLModelIRI.propertyTypeIRI(serviceType+resourceType,propertyName) ) )
      .findFirst().toScala match {
      case None => None
      case Some(ax) => Some(ax.datatypesInSignature().findFirst().get().getBuiltInDatatype)
    }
  }

  def objectPropertyRange ( propertyName:String ) : Option[OWLClass]  = {
    resourceOntology.objectPropertyRangeAxioms(
      ssE.manager.getOWLDataFactory.getOWLObjectProperty( DLModelIRI.propertyTypeIRI(serviceType+resourceType,propertyName) ))
      .findFirst().toScala match {
      case None => None
      case Some(ax) =>
        Some( ax.classesInSignature().findFirst().get() )}
  }

  def resourceOntology: OWLOntology = ssE.manager.getOntology(DLModelIRI.resourceTerminologyIRI(serviceType+resourceType))





  private def valueNodeFromSingleAttribute(attributeNode:Json) : GenericValueNode = {
    if (attributeNode.isString)
      StringNode( DecodeJson.StringDecodeJson.decodeJson(attributeNode).toOption.get )
    else if (attributeNode.isNumber)
      LongNode( DecodeJson.LongDecodeJson.decodeJson(attributeNode).toOption.get )
    else if (attributeNode.isBool)
      BooleanNode( DecodeJson.BooleanDecodeJson.decodeJson(attributeNode).toOption.get )
    else if (attributeNode.isArray)
      ListNode ( (attributeNode.array.get map valueNodeFromSingleAttribute).toVector )
    else // TODO This is in case the attribute is an object. SHOULD NOT HAPPEN
      StringNode( DecodeJson.StringDecodeJson.decodeJson(attributeNode).toOption.get )
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

  private def importResourceSpecificationOntology: Unit  = {
    ssE.manager.loadOntologyFromOntologyDocument(
      new File("src/main/resources/terminology/resourcespecificationsOwl/"
        + serviceType + resourceType + ".owl"))
  }


}
