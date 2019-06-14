package aws.cfn.model

import org.semanticweb.owlapi.model.IRI
import java.util.UUID.randomUUID

import aws.cfn.{AwsOntology, Extension, Ontology, OntologySuffix}


object ModelIRI {


  def resourceTerminologyIRI(serviceResourceName: String): IRI =
    iri(serviceResourceName)

  
  def resourceTerminologyNamespace(serviceName: String, resourceName: String): String =
    serviceName.toLowerCase + resourceName.toLowerCase

  
  def filename (serviceName: String, resourceName: String) : String =
    serviceName.toLowerCase + resourceName.toLowerCase + Extension.Owl


  def resourceTypeIRI(resSpecName: String, resID: String): IRI =
    iri(resSpecName, resID)

  
  def subpropertyTypeIRI(resSpecName: String, subpropName: String): IRI =
    iri(resSpecName,subpropName)

  
  def attributeTypeIRI(resSpecName: String, attrName: String): IRI =
    iri(resSpecName,attrName)

  
  def propertyTypeIRI(resSpecName: String, propertyName: String): IRI =
    iri(resSpecName,propertyName)


  def mapEntryConceptIRI(resSpecName: String, valueType: String): IRI =
    iri(resSpecName, 
      Ontology.MapEntryPrefix + valueType)

  
  def mapEntryKeyRoleIRI(resSpecName: String, valueType: String): IRI =
    iri(resSpecName,
      Ontology.MapEntryPrefix + valueType + Ontology.MapKeySuffix)

  
  def mapEntryValueRoleIRI(resSpecName: String, valueType: String): IRI =
    iri(resSpecName, 
      Ontology.MapEntryPrefix + valueType + Ontology.MapValueSuffix)


  def actionsOntologyIRI(serviceName: String): IRI =
    iri(serviceName)

  
  def actionIRI(serviceName: String, fullActionName: String): IRI =
    iri(serviceName +
      OntologySuffix.Actions, 
      fullActionName.split(":").last)


  def stackSetIRI(stacksetName: String): IRI =
    iri(stacksetName + OntologySuffix.StackSet)

  
  def stackSetNamespace(stacksetName: String): String =
    stacksetName.toLowerCase

  
  def resourceInstanceIRI(stacksetName: String, resID: String): IRI =
    iri(stacksetName,resID)

  
  def subpropertyBlankNodeIRI(stacksetName: String): IRI =
    iri(stacksetName, 
      Ontology.SubpropertyIndividualPrefix + randomUUID)

  
  def policyNodeIRI(stacksetName: String): IRI =
    iri(stacksetName, 
      Ontology.PolicyDocumentIndividualPrefix + randomUUID)

  
  def embeddedPolicyIRI(stacksetName: String): IRI =
    iri(stacksetName, 
      Ontology.EmbeddedPolicyIndividualPrefix+randomUUID )


  def infrastructureModelIRI(infrastrName: String): IRI =
    iri(infrastrName + OntologySuffix.Infrastructure)

  
  def externalEntityIRI(infrastrName: String, extResName: String): IRI =
    iri(infrastrName + 
      OntologySuffix.Infrastructure, extResName)

  
  def servicePrincipalIRI(infrastrName: String, servPrincName: String): IRI =
    iri(infrastrName +
      OntologySuffix.Infrastructure, servPrincName)


  def permissionsModelIRI(infrastrName: String): IRI =
    iri(infrastrName + OntologySuffix.Permissions)

  
  def awsPublicIRI: IRI =
    awsIri(AwsOntology.Public)

  
  def awsServicePrincipalIRI(servPrincName : String): IRI =
    awsIri(servPrincName)

  
  def awsFederatedAccountIRI(federation: String): IRI =
    awsIri(federation)

  
  def awsAccountIRI(accountId: String): IRI =
    awsIri(accountId)

  
  def awsCanonicalUserIRI(canonicalUserId: String): IRI =
    awsIri(canonicalUserId)


  def awsManagedPolicyIRI(arn: String): IRI =
    awsIri(arn)

  
  def awsPropertyIRI(propertyName:String): IRI =
    awsIri(propertyName)

  
  def awsConceptIRI(conceptName: String): IRI =
    awsIri(conceptName)

  
  def awsIndividualIRI(individualName: String): IRI =
    awsIri(individualName)


  private def iri(ontologyName:String, entityName:String = "") =
    IRI.create( 
      Ontology.VersionStringIRI +
        ontologyName.toLowerCase +
        Ontology.Pound + 
        entityName.toLowerCase )

  
  private def awsIri(entityName: String ) =
    IRI.create(
      AwsOntology.StringIRI + 
        Ontology.Pound + 
        entityName.toLowerCase)

  
}

