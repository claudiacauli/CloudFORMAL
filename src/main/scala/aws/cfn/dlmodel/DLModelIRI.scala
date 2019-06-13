package aws.cfn.dlmodel

import org.semanticweb.owlapi.model.IRI
import java.util.UUID.randomUUID


object DLModelIRI {



  val baseIRI : IRI = IRI.create("http://www.claudiacauli.com/aws/cfn/2019/")
  val versionIRI : IRI = IRI.create(baseIRI.toString + "v1.0/")
  val currentIRI : IRI = versionIRI




  /*
    Specification Alphabet Naming Rules
   */
  def resourceTerminologyIRI : String => IRI =
    serviceResourceName => IRI.create(currentIRI.toString+ serviceResourceName.toLowerCase() + "#")

  def resourceTerminologyNamespace : (String, String) => String =
    (serviceName, resourceName) => serviceName.toLowerCase()+resourceName.toLowerCase()

  def filename : (String,String) => String =
    (serviceName, resourceName) => serviceName.toLowerCase()+resourceName.toLowerCase()+".owl"




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




  def actionsOntologyIRI : String => IRI =
    serviceName => IRI.create( currentIRI.toString + serviceName.toLowerCase() + "#" )

  def actionIRI : (String,String) => IRI =
    (serviceName,actionName) => IRI.create(currentIRI.toString+serviceName.toLowerCase+"Actions#"
      +actionName.split(":").last.toLowerCase())

  def policyDocIRI:IRI =
    IRI.create(currentIRI.toString + "policydocument#policydocument")





  /*
    Template Alphabet Naming Rules
  */
  def stackSetIRI : String => IRI =
    stackSetName => IRI.create(currentIRI.toString + stackSetName.toLowerCase() + "_stackset#")

  def stackSetNamespace : String => String =
    stackSetName => stackSetName.toLowerCase()

  def resourceInstanceIRI : (String,String) => IRI =
    (stackSetName,resourceLogicalId) => IRI.create(currentIRI.toString + stackSetName.toLowerCase() + "#" + resourceLogicalId.toLowerCase)

  def subpropertyBlankNodeIRI : String => IRI =
    stackSetName => IRI.create(currentIRI.toString + stackSetName.toLowerCase + "#" + "subproperty_" + randomUUID )

  def policyNodeIRI : String => IRI =
    stackSetName => IRI.create(currentIRI.toString + stackSetName.toLowerCase + "#" + "policydocument_"+randomUUID )

  def embeddedPolicyIRI : String => IRI =
    stackSetName => IRI.create(currentIRI.toString + stackSetName.toLowerCase + "#" + "embeddedpolicy_" + randomUUID())





  def stackFlowIRI : String => IRI =
    stackSetName => IRI.create(currentIRI.toString + stackSetName.toLowerCase() + "_flow#")

  def infrastructureModelIRI : String => IRI =
    infrastructureName => IRI.create(currentIRI.toString + infrastructureName.toLowerCase + "_infrastructure#")

  def externalEntityIRI    : (String,String) => IRI =
    (infrastructureName, externalEntityName) =>  IRI.create(currentIRI.toString + infrastructureName.toLowerCase() +
      "_infrastructure#" + externalEntityName.toLowerCase)

  def servicePrincipalIRI : (String,String) => IRI =
    (infrastructureName, servicePrincipalName) =>  IRI.create(currentIRI.toString + infrastructureName.toLowerCase() +
      "_infrastructure#" + servicePrincipalName.toLowerCase)

  def permissionsModelIRI : String => IRI =
    infrastructureName => IRI.create(currentIRI.toString + infrastructureName.toLowerCase + "_permissions#")

  def awsPublicIRI: IRI =
    IRI.create(currentIRI.toString + "aws#Public" )

  def awsServicePrincipalIRI : String => IRI =
    servicePrincipal => IRI.create(currentIRI.toString + "aws#"+servicePrincipal)

  def awsFederatedAccountIRI : String => IRI =
    federation => IRI.create(currentIRI.toString + "aws#"+federation)

  def awsAccountIRI : String => IRI =
    accountId => IRI.create(currentIRI.toString + "aws#"+accountId)

  def awsCanonicalUserIRI : String => IRI =
    canonicalUser => IRI.create(currentIRI.toString + "aws#"+canonicalUser)

  def awsPropertyIRI : String => IRI =
    propertyName => IRI.create(currentIRI.toString + "aws#"+propertyName)

  def awsConceptIRI : String => IRI =
    conceptName => IRI.create(currentIRI.toString + "aws#"+conceptName)

  def awsIndividualIRI : String => IRI =
    individualName => IRI.create(currentIRI.toString + "aws#"+individualName)

}