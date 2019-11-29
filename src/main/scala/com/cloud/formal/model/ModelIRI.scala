/*
 *    Copyright 2019 Claudia Cauli
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */

package com.cloud.formal.model

import org.semanticweb.owlapi.model.IRI
import java.util.UUID.randomUUID

import com.cloud.formal.{AwsOntology, Extension, Ontology, OntologySuffix}


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

  
  def resourceInstanceIRI(stacksetName: String,
                          templateName: String, resID: String): IRI =
    iri(stacksetName, stacksetName + "_" + templateName+"_"+resID)

  
  def subpropertyBlankNodeIRI(stacksetName: String): IRI =
    iri(stacksetName, 
      Ontology.SubpropertyIndividualPrefix + randomUUID)


  def infrastructureModelIRI(infrastrName: String): IRI =
    iri(infrastrName + OntologySuffix.Infrastructure)

  
  def externalEntityIRI(infrastrName: String, extResName: String): IRI =
    iri(infrastrName + 
      OntologySuffix.Infrastructure, extResName)


  
  def awsAccountIRI(accountId: String): IRI =
    awsIri(accountId)

  
  def awsCanonicalUserIRI(canonicalUserId: String): IRI =
    awsIri(canonicalUserId)

  
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

