package aws.cfn.dlmodel

import org.semanticweb.owlapi.model.IRI
import java.util.UUID.randomUUID


object Symbols {



  val baseIRI : IRI = IRI.create("https://www.claudiacauli.com/aws/cfn/2019/")
  val versionIRI : IRI = IRI.create(baseIRI + "v1.0/")
  val currentIRI : IRI = versionIRI




  /*
    Specification Alphabet Naming Rules
   */
  def resourceTerminologyIRI : String => IRI = serviceResourceName => IRI.create(currentIRI+ serviceResourceName + "#")
  def resourceTerminologyNamespace : (String, String) => String = (serviceName, resourceName) => serviceName+resourceName
  def filename : (String,String) => String = (serviceName, resourceName) => serviceName+resourceName+".owl"

  def resourceTypeIRI :(String,String) => IRI =
    (resSpecName, resourceName) => IRI.create(currentIRI + resSpecName +"#" + resourceName)
  def subpropertyTypeIRI : (String,String) => IRI =
    (resSpecName,subpropertyName) => IRI.create(currentIRI + resSpecName + "#" + subpropertyName)
  def attributeTypeIRI : (String,String) => IRI =
    (resSpecName, attributeName) => IRI.create(currentIRI + resSpecName + "#" + attributeName)
  def propertyTypeIRI : (String,String) => IRI =
    (resSpecName, propertyName) => IRI.create(currentIRI + resSpecName + "#" + propertyName)

  def mapEntryConceptIRI : (String,String) => IRI =
    (resSpecName, valueType) => IRI.create(currentIRI + resSpecName + "#" + "mapentry_" + valueType)
  def mapEntryKeyRoleIRI : (String,String) => IRI =
    (resSpecName, valueType) => IRI.create(currentIRI + resSpecName + "#" + "mapentry_" + valueType + "_key")
  def mapEntryValueRoleIRI : (String,String) => IRI =
    (resSpecName, valueType) => IRI.create(currentIRI + resSpecName + "#" + "mapentry_" + valueType + "_value")

  def actionsOntologyIRI : String => IRI = serviceName => IRI.create( currentIRI + serviceName + "#" )
  def actionIRI : (String,String) => IRI = (serviceName,actionName) => IRI.create(currentIRI+serviceName+"#"+actionName)



  /*
    Template Alphabet Naming Rules
  */
  def stackSetIRI : String => IRI = stackSetName => IRI.create(currentIRI + stackSetName.toLowerCase() + "#")
  def stackSetNamespace : String => String = stackSetName => stackSetName.toLowerCase()
  def resourceInstanceIRI : (String,String) => IRI = (stackSetName,resourceLogicalId) => IRI.create(currentIRI + stackSetName.toLowerCase() + "#" + resourceLogicalId.toLowerCase)
  def subpropertyBlankNodeIRI : String => IRI = stackSetName => IRI.create(currentIRI + stackSetName.toLowerCase + "#" + "uid_" + randomUUID )



}
