package aws.cfn.dlmodel

import org.semanticweb.owlapi.vocab.OWL2Datatype

object PrimitiveTypes {

  val OWL_STRING = OWL2Datatype.XSD_STRING
  val OWL_LONG = OWL2Datatype.XSD_LONG
  val OWL_BOOL = OWL2Datatype.XSD_BOOLEAN
  val OWL_INT = OWL2Datatype.XSD_INT
  val OWL_FLOAT = OWL2Datatype.XSD_FLOAT
  val OWL_DOUBLE = OWL2Datatype.XSD_DOUBLE

  def fromString(primType : String) : OWL2Datatype = primType match {
    case "string" => PrimitiveTypes.OWL_STRING
    case "boolean" => PrimitiveTypes.OWL_BOOL
    case "float" => PrimitiveTypes.OWL_FLOAT
    case "double" => PrimitiveTypes.OWL_DOUBLE
    case "integer" => PrimitiveTypes.OWL_INT
    case "long" => PrimitiveTypes.OWL_LONG
    case _ => PrimitiveTypes.OWL_STRING
  }

  def toString(dataType : OWL2Datatype) : String = dataType match {
    case OWL_STRING => "string"
    case OWL_LONG => "long"
    case OWL_BOOL => "boolean"
    case OWL_INT => "integer"
    case OWL_FLOAT => "float"
    case OWL_DOUBLE => "double"
    case _ => "string"
  }

}
