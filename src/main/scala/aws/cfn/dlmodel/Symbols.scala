package aws.cfn.dlmodel

import org.semanticweb.owlapi.model.IRI
import java.util.UUID.randomUUID


object Symbols {



  val baseIRI : IRI = IRI.create("http://www.claudiacauli.com/aws/cfn/2019/")
  val versionIRI : IRI = IRI.create(baseIRI.toString + "v1.0/")
  val currentIRI : IRI = versionIRI




  /*
    Specification Alphabet Naming Rules
   */
  def resourceTerminologyIRI : String => IRI = serviceResourceName => IRI.create(currentIRI.toString+ serviceResourceName.toLowerCase() + "#")
  def resourceTerminologyNamespace : (String, String) => String = (serviceName, resourceName) => serviceName.toLowerCase()+resourceName.toLowerCase()
  def filename : (String,String) => String = (serviceName, resourceName) => serviceName.toLowerCase()+resourceName.toLowerCase()+".owl"

  def resourceTypeIRI :(String,String) => IRI =
    (resSpecName, resourceName) => IRI.create(currentIRI.toString + resSpecName.toLowerCase() +"#" + resourceName.toLowerCase())
  def subpropertyTypeIRI : (String,String) => IRI =
    (resSpecName,subpropertyName) => IRI.create(currentIRI.toString + resSpecName.toLowerCase() + "#" + subpropertyName.toLowerCase())
  def attributeTypeIRI : (String,String) => IRI =
    (resSpecName, attributeName) => IRI.create(currentIRI.toString + resSpecName.toLowerCase() + "#" + attributeName.toLowerCase())
  def propertyTypeIRI : (String,String) => IRI =
    (resSpecName, propertyName) => IRI.create(currentIRI.toString + resSpecName.toLowerCase() + "#" + propertyName.toLowerCase())

  def mapEntryConceptIRI : (String,String) => IRI =
    (resSpecName, valueType) => IRI.create(currentIRI.toString + resSpecName.toLowerCase() + "#" + "mapentry_" + valueType.toLowerCase())
  def mapEntryKeyRoleIRI : (String,String) => IRI =
    (resSpecName, valueType) => IRI.create(currentIRI.toString + resSpecName.toLowerCase() + "#" + "mapentry_" + valueType.toLowerCase() + "_key")
  def mapEntryValueRoleIRI : (String,String) => IRI =
    (resSpecName, valueType) => IRI.create(currentIRI.toString + resSpecName.toLowerCase() + "#" + "mapentry_" + valueType.toLowerCase() + "_value")

  def actionsOntologyIRI : String => IRI = serviceName => IRI.create( currentIRI.toString + serviceName.toLowerCase() + "#" )
  def actionIRI : (String,String) => IRI = (serviceName,actionName) => IRI.create(currentIRI.toString+serviceName.toLowerCase()+"#"+actionName.toLowerCase())
  def policyDocIRI:IRI = IRI.create(currentIRI.toString + "policydocument#policydocument")


  /*
    Template Alphabet Naming Rules
  */
  def stackSetIRI : String => IRI = stackSetName => IRI.create(currentIRI.toString + stackSetName.toLowerCase() + "#")
  def stackSetNamespace : String => String = stackSetName => stackSetName.toLowerCase()
  def resourceInstanceIRI : (String,String) => IRI = (stackSetName,resourceLogicalId) => IRI.create(currentIRI.toString + stackSetName.toLowerCase() + "#" + resourceLogicalId.toLowerCase)
  def subpropertyBlankNodeIRI : String => IRI = stackSetName => IRI.create(currentIRI.toString + stackSetName.toLowerCase + "#" + "Subproperty_" + randomUUID )
  def policyNodeIRI : String => IRI = stackSetName => IRI.create(currentIRI.toString + stackSetName.toLowerCase + "#" + "Policy_"+randomUUID )


}
