package aws.cfn.templates

import java.util.regex.Pattern

import argonaut.{DecodeJson, Json}
import aws.cfn.actions.ActionsMap
import aws.cfn.dlmodel.{ModelIRI, ModelType, ModelUtils}
import aws.cfn.specifications.{CFnType, Renaming, Specification}
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.vocab.OWL2Datatype

import scala.jdk.OptionConverters._

protected object Json2ResourceEncoder {


  def subPropertiesNamesOfClassName (className: String, ssE: Json2StackSetEncoder,
                                     serviceType: String, resourceType: String,
                                     resourceOntology: OWLOntology): Set[String] = {


    def subPropertiesOfClassName  = {
      val classObj = Some(ssE.df.getOWLClass(ModelIRI.resourceTypeIRI(serviceType+resourceType,className)))
      subPropertiesOf(classObj)
    }

    def allPropertiesInOntology =
      resourceOntology.dataPropertiesInSignature().toArray().toVector ++
        resourceOntology.objectPropertiesInSignature().toArray().toVector

    def propertyNameStartsWithClassName(p:AnyRef, className:String) =
      p match {
      case property: OWLObjectProperty
        => property.getIRI.toString.split("#").last.startsWith(className + Renaming.Delimiter)
      case _
        => p.asInstanceOf[OWLDataProperty].getIRI.toString.split(Ontology.Pound).last.startsWith(className + Renaming.Delimiter)
    }

    def subPropertiesOf(classObj:Option[OWLClass]) = {
      classObj match {
        case None     => Vector()
        case Some(c)  =>
          val className = c.getIRI.toString.split(Ontology.Pound).last
          (allPropertiesInOntology filter (p => propertyNameStartsWithClassName(p, className)))
            .map {
              case property: OWLDataProperty => Left(property)
              case p => Right(p.asInstanceOf[OWLObjectProperty])
            }
      }
    }




    (subPropertiesOfClassName map {
      case Left(dp)   => dp.getIRI.toString.split(Ontology.Pound).last
      case Right(op)  => op.getIRI.toString.split(Ontology.Pound).last
    }).toSet
  }




  def rangeNameOf(propertyName:String, resourceOntology: OWLOntology,
                  serviceType: String, resourceType: String,
                  ssE: Json2StackSetEncoder): Option[(String,String)] = {

    def rangeOf(propertyName:String) : Option[Serializable] = {
      if (isObjectProperty(propertyName)) objectPropertyRange(propertyName) match {
        case None => Some(CFnType.UnknownType)
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
        ModelIRI.propertyTypeIRI(serviceType+resourceType, propertyName ))
    }

    def isDataProperty( propertyName:String ): Boolean = {
      resourceOntology.containsDataPropertyInSignature(
        ModelIRI.propertyTypeIRI(serviceType+resourceType, propertyName ))
    }

    def dataPropertyRange( propertyName:String ): Option[OWL2Datatype] = {
      resourceOntology.dataPropertyRangeAxioms(
        ssE.df.getOWLDataProperty( ModelIRI.propertyTypeIRI(serviceType+resourceType,propertyName) ) )
        .findFirst().toScala match {
        case None => None
        case Some(ax) => Some(ax.datatypesInSignature().findFirst().get().getBuiltInDatatype)
      }
    }

    def objectPropertyRange ( propertyName:String ) : Option[OWLClass]  = {
      resourceOntology.objectPropertyRangeAxioms(
        resourceOntology.getOWLOntologyManager.getOWLDataFactory.getOWLObjectProperty( ModelIRI.propertyTypeIRI(serviceType+resourceType,propertyName) ))
        .findFirst().toScala match {
        case None => None
        case Some(ax) =>
          Some( ax.classesInSignature().findFirst().get() )}
    }





    rangeOf(propertyName) match {
      case Some(CFnType.UnknownType) => Some((CFnType.UnknownService, CFnType.UnknownResource))
      case Some(Left(dt:OWL2Datatype)) => Some(("",ModelType.stringOf(dt)))
      case Some(Right(c:OWLClass)) => Some((c.getIRI.toString.split(Ontology.Pound).head.split("/").last,
        c.getIRI.toString.split(Ontology.Pound).last))
      case _ => None
    }
  }


}


protected class Json2ResourceEncoder(val tE:Json2TemplateEncoder,
                           resourceLogicalId:String, val resourceJsonNode:Json) {

  val parentStackSetEncoder: Json2StackSetEncoder             = tE.ssE
  val parentInfrastructureEncoder: Json2InfrastructureEncoder = parentStackSetEncoder.iE

  val serviceType: String   = getServiceName
  val resourceType: String  = getResourceType
  val NodeEncoder     = new Json2NodeEncoder(this)
  var resource : StackSetResource = _





  def createResourceNodeWithAttributes: Map[String,StackSetResource] = {

    if (tE.hasTrueCondition(resourceJsonNode)){
      resource = StackSetResource( resourceLogicalId, serviceType, resourceType, tE.ssE.stackSet, attributesFromResourceJsonNode )
      importResourceSpecificationOntology()
      Map( resourceLogicalId -> resource )
    }
    else Map()

  }

  def updateResourceName() : Unit = {
    if (tE.hasTrueCondition(resourceJsonNode))
      resource.resourceName = getResourceName
  }


  def deepInstantiationOfResource(): StackSetResource = {

    updateResourceByPolicy()

    resource.absentProperties =
      Json2ResourceEncoder.subPropertiesNamesOfClassName(resourceType.toLowerCase(),parentStackSetEncoder,serviceType,resourceType,resourceOntology) --
      givenProperties.map(s=>s.toLowerCase())

    resource.givenProperties =
      (givenProperties flatMap (propName => Map(propName ->
      nodeObjectForProperty(propName, serviceType,resourceType, parentStackSetEncoder, resourceOntology)))).toMap


    resource

  }





  def resourceOntology: OWLOntology = parentStackSetEncoder.manager.getOntology(ModelIRI.resourceTerminologyIRI(serviceType+resourceType))








  private def givenProperties: Set[String] =
    (resourceJsonNode.field(Specification.Properties).getOrElse(Json.jEmptyObject)
      .objectFieldsOrEmpty map (f => resourceType+ Renaming.Delimiter +f.toString)).toSet


  private def nodeObjectForProperty(propFullName: String, serviceType: String, resourceType:String,
                                    ssE :Json2StackSetEncoder, resourceOntology: OWLOntology) : Node = {

    val propTemplateName  = propFullName.split(resourceType+ Renaming.Delimiter).last
    val propContentNode   = resourceJsonNode.field(Specification.Properties).get.field(propTemplateName).get
    val propContentType   = Json2ResourceEncoder.rangeNameOf(propFullName, resourceOntology, serviceType, resourceType, ssE)

    NodeEncoder.encode(propContentNode,propContentType)

  }


  private def isCustomResource : Boolean =
    Pattern
      .compile(Specification.CustomResourceRegex,Pattern.CASE_INSENSITIVE)
      .matcher(resourceJsonNode.field(Specification.Type).get.string.get)
      .matches()


  private def getServiceName: String =
    if ( isCustomResource )   Specification.CustomServiceType
    else      resourceJsonNode.field(Specification.Type).get.string.get.split("::")(1)


  private def getResourceType: String =
    if ( isCustomResource )   Specification.CustomResourceType
    else      resourceJsonNode.field(Specification.Type).get.string.get.split("::")(2)


  private def getResourceName: String = {
    ResourcesNameFieldsMap.lookUp(serviceType,resourceType) match {
      case None     => resourceLogicalId
      case Some(f)  =>
        if (resourceJsonNode.field(Specification.Properties).get.field(f).isDefined)
          NodeEncoder.encode( resourceJsonNode.field(Specification.Properties).get.field(f).get ) match {
            case StringNode(s) => s
            case _ => resourceLogicalId
          }
        else
          resourceLogicalId
    }
  }



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
    resourceJsonNode.field(Specification.Attributes) match {
      case None => Map()
      case Some(attributesNode) =>
        (attributesNode.objectFieldsOrEmpty flatMap
          ( f => Map(f.toString.toLowerCase() -> valueNodeFromSingleAttribute(attributesNode.field(f).get)))
          ).toMap
    }
  }


  private def importResourceSpecificationOntology() : Unit  = {
    ModelUtils
      .loadResourceSpecificationModelFromSpec(
        serviceType,resourceType,parentStackSetEncoder.manager)

    ActionsMap.getActionPrefixFromService(serviceType.toLowerCase)
      .foreach( aPref =>
        ModelUtils
          .loadActionsModelFromSpec(aPref,parentStackSetEncoder.manager)
      )
  }



  def pointedResourceIsPolicy(res : Node) : Boolean = {
    res match {
      case ssR: StackSetResource => (ssR.serviceType.toLowerCase, ssR.resourceType.toLowerCase) match {
        case ("s3", "bucketpolicy") => true
        case ("iam", "managedpolicy") => true
        case ("iam", "policy") => true
        case ("iot", "policy") => true
        case ("sqs", "queuepolicy") => true
        case ("sns", "topicpolicy") => true
        case ("secretsmanager", "resourcepolicy") => true
        case _ => false
      }
      case _ => false
    }
  }


  def currentResourceIsPolicyAndPointsTo : Set[StackSetResource] = {
    (resource.serviceType.toLowerCase,resource.resourceType.toLowerCase) match {
      case ("s3","bucketpolicy")    =>
        if (resourceJsonNode.field(Specification.Properties).get.field("Bucket").isDefined){
          NodeEncoder.encode(resourceJsonNode.field(Specification.Properties).get.field("Bucket").get) match {
            case r:StackSetResource => Set( r )
            case _ => Set()
          }
        }
        else Set()
        case _ => Set()
    }
  }

  def updateResourceByPolicy(): Unit = {
    currentResourceIsPolicyAndPointsTo foreach (pr => parentInfrastructureEncoder.updateResByPolicyMap(resource, pr))
  }

}
