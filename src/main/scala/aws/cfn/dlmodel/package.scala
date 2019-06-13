package aws.cfn

import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.vocab.OWL2Datatype


package object dlmodel{


  private[dlmodel]
  object Format extends  Enumeration {
    val Rdf         = "rdf"
    val Xml         = "xml"
    val Turtle      = "ttl"
    val Functional  = "fun"
    val DefaultFormat: String = Rdf
  }


  private[dlmodel]
  object Extension extends Enumeration {
    val Owl   = ".owl"
  }


  private[dlmodel]
  object ModelSuffix extends Enumeration {
    val StackSet        = "_StackSetModel"
    val Infrastructure  = "_InfrastructureModel"
    val Permissions     = "_PermissionsModel"
  }


  private[dlmodel]
  object ModelFileSuffix extends Enumeration {
    val StackSet: String        = ModelSuffix.StackSet + Extension.Owl
    val Infrastructure: String  = ModelSuffix.Infrastructure + Extension.Owl
    val Permissions: String     = ModelSuffix.Permissions + Extension.Owl
  }


  private[dlmodel]
  object OntologySuffix extends Enumeration{
    val Actions         = "actions"
    val StackSet        = "_stackset"
    val Infrastructure  = "_infrastructure"
    val Permissions     = "_permissions"
  }


  private[dlmodel]
  object ProtegeCatalogue {
    val FileName: String = "catalog-v001.xml"
  }


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


  object AwsOntology {
    val Name  = "aws"
    val StringIRI:String  = Ontology.VersionIRI + Name
    val Pound: String     = Ontology.Pound

    val Public            = "Public"
    val ExternalResource  = "ExternalResource"
    val Account           = "AwsAccount"
    val Federation        = "Federation"

    val HasAccountWithFederation  = "hasAccountWithFederation"
    val AccessAccount             = "hasAccessToAccount"
    val IsOwnedByAccount          = "isOwnedByAccount"
    val IsDeployedIn              = "isDeployedIn"
    val IsInStack                 = "isInStack"

    val ResourceAnnotationComment     = "Resource"
    val SubpropertyAnnotationComment  = "Subproperty"
    val StackSuffix                   = "Stack"
  }


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


}

