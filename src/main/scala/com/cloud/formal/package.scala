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

package com.cloud

import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.vocab.OWL2Datatype


package object formal
{


  private[formal]
  object Extension extends Enumeration {
    val Json  = ".json"
    val Owl   = ".owl"
  }


  private[formal]
  object OntologySuffix extends Enumeration{
    val Actions         = "actions"
    val StackSet        = "_stackset"
    val Infrastructure  = "_infrastructure"
    val Permissions     = "_permissions"
  }


  private[formal]
  object FilePath {
    private val ActionsTermsPathPrefix  = "src/main/resources/terminology/actions/"
    private val ResourceTerms           = "src/main/resources/terminology/resourcespecificationsOwl/"
    val AwsOntology                     = "src/main/resources/terminology/aws.owl"

    val ActionTerminology: String => String
    = actionPrefix  =>
      ActionsTermsPathPrefix + actionPrefix + OntologySuffix.Actions + Extension.Owl

    val ResourceTerminology: (String,String) => String
    = (service,resource) =>
      ResourceTerms + service + resource + Extension.Owl
  }


  private[formal]
  object ModelType {

    val OwlString   = OWL2Datatype.XSD_STRING
    val OwlLong     = OWL2Datatype.XSD_LONG
    val OwlBool     = OWL2Datatype.XSD_BOOLEAN
    val OwlInt      = OWL2Datatype.XSD_INT
    val OwlFloat    = OWL2Datatype.XSD_FLOAT
    val OwlDouble   = OWL2Datatype.XSD_DOUBLE

    def fromString(stringType : String) : OWL2Datatype =
      stringType match {
        case "boolean"  => OwlBool
        case "float"    => OwlFloat
        case "double"   => OwlDouble
        case "integer"  => OwlInt
        case "long"     => OwlLong
        case _          => OwlString
      }

    def stringOf(owlDataType : OWL2Datatype) : String =
      owlDataType match {
        case OwlLong    => "long"
        case OwlBool    => "boolean"
        case OwlInt     => "integer"
        case OwlFloat   => "float"
        case OwlDouble  => "double"
        case _          => "string"
      }
  }


  private[formal]
  object Ontology {
    val BaseStringIRI   = "http://www.claudiacauli.com/aws/cfn/2019/"
    val Version         = "1.0"
    val VersionStringIRI: String  = BaseStringIRI + "v" + Version + "/"
    val VersionIRI : IRI          = IRI.create(VersionStringIRI)
    val Pound = "#"

    val MapEntryPrefix    = "mapentry_"
    val MapKeySuffix      = "_key"
    val MapValueSuffix    = "_value"

    val SubpropertyIndividualPrefix     = "subproperty_"
    val PolicyDocumentIndividualPrefix  = "policydocument_"
    val EmbeddedPolicyIndividualPrefix  = "embeddedpolicy_"
  }


  private[formal]
  object AwsOntology {
    val Name  = "aws"
    val StringIRI:String  = Ontology.VersionIRI + Name
    val Pound: String     = Ontology.Pound

    val Public            = "Public"
    val ExternalResource  = "ExternalResource"
    val Account           = "AwsAccount"
    val Federation        = "Federation"
    val AwsManagedPolicy  = "AwsManagedPolicy"

    val HasAccountWithFederation  = "hasAccountWithFederation"
    val AccessAccount             = "hasAccessToAccount"
    val IsOwnedByAccount          = "isOwnedByAccount"
    val IsDeployedIn              = "isDeployedIn"
    val IsInStack                 = "isInStack"

    val ResourceAnnotationComment     = "Resource"
    val SubpropertyAnnotationComment  = "Subproperty"
    val StackSuffix                   = "Stack"
  }


}

