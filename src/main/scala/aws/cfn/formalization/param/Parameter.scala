//package aws.cfn.aws.cfn.types.types.param
//
//import aws.cfn.aws.cfn.types.types.{CloudFormationValue, ListValue, StringValue}
//import org.apache.commons.lang3.RandomStringUtils
//import argonaut._
//
//sealed trait Parameter [ T <: CloudFormationValue ] {
//  val value : Option[T]
//  val name : String
//}
//
//
//sealed trait PseudoParameter[T <: CloudFormationValue] extends Parameter[T]
//
//
//
//
//sealed trait StringPseudoParameter extends PseudoParameter[ StringValue ]
//
//  case class AWSRegion ( s : String = "us-east-1" ) extends StringPseudoParameter {
//    val value = Some ( StringValue( s ) )
//    val name = "AWSRegion"
//  }
//
//  case class AWSPartition ( s : String = "us-east-1" ) extends StringPseudoParameter {
//    val value = Some ( StringValue( s ) )
//    val name = "AWSPartition"
//  }
//
//  case class AWSAccountId ( s : String = "uid" + RandomStringUtils.randomNumeric(9) ) extends StringPseudoParameter {
//    val value = Some ( StringValue( s ) )
//    val name = "AWSAccountId"
//  }
//
//  case class AWSStackId ( s : String = "uid" + RandomStringUtils.randomAlphanumeric(9) ) extends StringPseudoParameter {
//    val value = Some ( StringValue( s ) )
//    val name = "AWSStackId"
//  }
//
//  case class AWSStackName (s : String = "Stack" + RandomStringUtils.randomNumeric(7) ) extends StringPseudoParameter {
//    val value = Some ( StringValue( s ) )
//    val name = "AWSStackName"
//  }
//
//  case class AWSURLSuffix ( s : String = "amazonaws.com" ) extends StringPseudoParameter {
//    val value = Some ( StringValue( s ) )
//    val name = "AWSURLSuffix"
//  }
//
//  case object AWSNoValue extends StringPseudoParameter {
//    val name = "AWSNoValue"
//    val value = None
//  }
//
//
//
//
//sealed trait ListPseudoParameter extends PseudoParameter [ ListValue[StringValue] ]
//
//  case class AWSNotificationARNs (value : Option[ ListValue[StringValue] ] = None ) extends ListPseudoParameter {
//    val name = "AWSNotificationARNs"
//  }
//
//
//
//class InputParameter[T <: CloudFormationValue](val name : String, val value : Option[T], val defaultValue : Option[T]) extends Parameter[T]