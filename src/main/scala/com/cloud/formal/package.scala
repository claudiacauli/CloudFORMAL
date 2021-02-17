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
  object Extension extends Enumeration
  {
    val JSON  = ".json"
    val OWL   = ".owl"
    val CSV   = ".csv"
    val DOT   = ".dot"
    val PNG   = ".png"
  }


  private[formal]
  object OntologySuffix extends Enumeration
  {
    val StackSet        = "_stackset"
    val Infrastructure  = "_infrastructure"
    val Aws             = "_aws"
  }

  private[formal]
  object SysUtil extends Enumeration
  {
    val UserHome        = "user.home"
    val CurrentDir      = "user.dir"
    val DSStore         = "DS_Store"
  }


  private[formal]
  object FileSuffix extends Enumeration
  {
    val Descriptor                = "Descriptor"
    val Report                    = "Report"
    val DescriptorJson: String    = Descriptor + Extension.JSON
    val ReportCsv: String         = Report + Extension.CSV
    val SpecificationJson: String = "Specification" + Extension.JSON
  }


  private[formal]
  object FilePath
  {
    val ProjectResources = "src/main/resources/"
    val ResourceSpecs = "src/main/resources/CloudFormationResourceSpecification/"
    val ResourceTerms = "src/main/resources/terminology/resourcespecificationsOwl/"
    val AwsOntology   = "src/main/resources/terminology/aws.owl"
    val DataflowSpecs = "src/main/resources/Dataflow/"
    val DataflowResSpecs = "src/main/resources/Dataflow/resourcespecificationsOwl/"
    val BenchmarksOut = "BenchmarksOut/"
    val BenchmarksIn  = "Benchmarks/"
    val DataflowOut = "DataflowOut/"
    val DataflowIn  = "DataflowIn/"

    val ResourceTerminology: (String,String) => String
    = (service,resource) =>
      ResourceTerms + service + resource + Extension.OWL
  }

  private[formal]
  object Renaming
  {
    val Delimiter = "_"
  }


  private[formal]
  object ModelType
  {

    val OwlString   = OWL2Datatype.XSD_STRING
    val OwlLong     = OWL2Datatype.XSD_LONG
    val OwlBool     = OWL2Datatype.XSD_BOOLEAN
    val OwlInt      = OWL2Datatype.XSD_INT
    val OwlFloat    = OWL2Datatype.XSD_FLOAT
    val OwlDouble   = OWL2Datatype.XSD_DOUBLE

    def fromString(stringType : String) : OWL2Datatype =
      stringType match
      {
        case "boolean"  => OwlBool
        case "float"    => OwlFloat
        case "double"   => OwlDouble
        case "integer"  => OwlInt
        case "long"     => OwlLong
        case _          => OwlString
      }

    def stringOf(owlDataType : OWL2Datatype) : String =
      owlDataType match
      {
        case OwlLong    => "long"
        case OwlBool    => "boolean"
        case OwlInt     => "integer"
        case OwlFloat   => "float"
        case OwlDouble  => "double"
        case _          => "string"
      }
  }


  private[formal]
  object Ontology
  {
    val BaseStringIRI   = "http://www.claudiacauli.com/aws/cfn/2019/"
    val Version         = "1.0"
    val VersionStringIRI: String  = BaseStringIRI + "v" + Version + "/"
    val VersionIRI : IRI          = IRI.create(VersionStringIRI)
    val Pound = "#"
    val OWLOntologyStringIRI      = "http://www.w3.org/2002/07/owl#"

    val MapEntryPrefix    = "mapentry_"
    val MapKeySuffix      = "_key"
    val MapValueSuffix    = "_value"

    val SubpropertyIndividualPrefix     = "subproperty_"
  }


  private[formal]
  object AwsOntology
  {
    val Name  = "aws"
    val StringIRI:String  = Ontology.VersionIRI.getIRIString + Name
    val Pound: String     = Ontology.Pound

    val ExternalResource  = "ExternalResource"

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

