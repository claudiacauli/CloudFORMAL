package aws.cfn.dlmodel

import org.semanticweb.owlapi.vocab.OWL2Datatype

object Constants {

  val OWL_STRING = OWL2Datatype.XSD_STRING
  val OWL_LONG = OWL2Datatype.XSD_LONG
  val OWL_BOOL = OWL2Datatype.XSD_BOOLEAN
  val OWL_INT = OWL2Datatype.XSD_INT
  val OWL_FLOAT = OWL2Datatype.XSD_FLOAT
  val OWL_DOUBLE = OWL2Datatype.XSD_DOUBLE

  def fromString(primType : String) : OWL2Datatype = primType match {
    case "string" => Constants.OWL_STRING
    case "boolean" => Constants.OWL_BOOL
    case "float" => Constants.OWL_FLOAT
    case "double" => Constants.OWL_DOUBLE
    case "integer" => Constants.OWL_INT
    case "long" => Constants.OWL_LONG
    case _ => Constants.OWL_STRING
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
