package aws.cfn


package object templates{

  private[templates]
  object Ontology extends Enumeration{
    val Pound = "#"
  }

  private[templates] object TemplateTag extends Enumeration {
    val Condition    = "Condition"
    val Conditions   = "Conditions"
    val Parameters   = "Parameters"
    val Transform    = "Transform"
    val Resources    = "Resources"
    val Mappings     = "Mappings"
    val Outputs      = "Outputs"
    val Default      = "Default"
    val Type         = "Type"
    val ValueTag     = "Value"
    val Name         = "Name"
    val Export       = "Export"
  }


  private[templates] object ParameterType {
    val String = "string"
    val Number = "number"
    val ListOfNumber = "list<number>"
    val CommaDelimitedList = "commadelimitedlist"
  }


  private[templates] object PseudoParameter {
    val AccountId         : String = "aws::accountid"
    val StackName         : String = "aws::stackname"
    val StackId           : String = "aws::stackid"
    val Partition         : String = "aws::partition"
    val Region            : String = "aws::region"
    val URLSuffix         : String = "aws::urlsuffix"
    val NotificationARNs  : String = "aws::notificationarns"
    val NoValue           : String = "aws::novalue"
  }


  private[templates] object CFnFunTag extends Enumeration {

    val FunTagRegex     = "^(Fn::|Ref).*"

    val Base64UC        = "Fn::Base64"
    val CidrUC          = "Fn::Cidr"
    val IfUC            = "Fn::If"
    val NotUC           = "Fn::Not"
    val AndUC           = "Fn::And"
    val EqualsUC        = "Fn::Equals"
    val OrUC            = "Fn::Or"
    val FindInMapUC     = "Fn::FindInMap"
    val GetAttUC        = "Fn::GetAtt"
    val GetAZsUC        = "Fn::GetAZs"
    val ImportValueUC   = "Fn::ImportValue"
    val JoinUC          = "Fn::Join"
    val SelectUC        = "Fn::Select"
    val SplitUC         = "Fn::Split"
    val SubUC           = "Fn::Sub"
    val TransformUC     = "Fn::Transform"
    val RefUC           = "Ref"

    val Base64: String      = Base64UC.toLowerCase
    val Cidr: String        = CidrUC.toLowerCase
    val If: String          = IfUC.toLowerCase
    val Not: String         = NotUC.toLowerCase
    val And: String         = AndUC.toLowerCase
    val Equals: String      = EqualsUC.toLowerCase
    val Or: String          = OrUC.toLowerCase
    val FindInMap: String   = FindInMapUC.toLowerCase
    val GetAtt: String      = GetAttUC.toLowerCase
    val GetAZs: String      = GetAZsUC.toLowerCase
    val ImportValue: String = ImportValueUC.toLowerCase
    val Join: String        = JoinUC.toLowerCase
    val Select: String      = SelectUC.toLowerCase
    val Split: String       = SplitUC.toLowerCase
    val Sub: String         = SubUC.toLowerCase
    val Transform: String   = TransformUC.toLowerCase
    val Ref: String         = RefUC.toLowerCase

  }

  object Policy extends Enumeration {
    val StatementTag      = "Statement"
    val EffectTag         = "Effect"
    val AllowValue        = "allow"
    val PrincipalTag      = "Principal "
    val NotPrincipalTag   = "NotPrincipal"
    val ActionTag         = "Action"
    val NotActionTag      = "NotAction"
    val ResourceTag       = "Resource"
    val NotResourceTag    = "NotResource"
    val ConditionTag      = "Condition"
    val StarValue         = "*"
    val ServiceTag        = "Service"
    val AWSTag            = "AWS"
    val FederatedTag      = "Federated"
    val CanonicalUserTag  = "CanonicalUser"
  }



  private[templates]
  object SummaryFileName extends Enumeration {
    val Infrastructure   = "InfrastructureSummary.txt"
    val Permissions      = "PermissionsSummary.txt"
  }

}


//object AwsPartition extends Enumeration {
//  val aws: AwsPartition.Value = Value("aws")
//  val awscn: AwsPartition.Value = Value("aws-cn")
//  val awsusgov: AwsPartition.Value = Value("aws-us-gov")
//}
//
//
//object AwsRegion extends Enumeration {
//  val useast1: AwsRegion.Value = Value("us-east-1")
//  val useast2: AwsRegion.Value = Value("us-east-2")
//  val uswest1: AwsRegion.Value = Value("us-west-1")
//  val uswest2: AwsRegion.Value = Value("us-west-2")
//  val apeast1: AwsRegion.Value = Value("ap-east-1")
//  val apsouth1: AwsRegion.Value = Value("ap-south-1")
//  val apnortheast3: AwsRegion.Value = Value("ap-northeast-3")
//  val apnortheast2: AwsRegion.Value = Value("ap-northeast-2")
//  val apsoutheast1: AwsRegion.Value = Value("ap-southeast-1")
//  val apsoutheast2: AwsRegion.Value = Value("ap-southeast-2")
//  val apnortheast1: AwsRegion.Value = Value("ap-northeast-1")
//  val cacentral1: AwsRegion.Value = Value("ca-central-1")
//  val cnnorth1: AwsRegion.Value = Value("cn-north-1")
//  val cnnorthwest1: AwsRegion.Value = Value("cn-northwest-1")
//  val euwest1: AwsRegion.Value = Value("eu-west-1")
//  val euwest2: AwsRegion.Value = Value("eu-west-2")
//  val euwest3: AwsRegion.Value = Value("eu-west-3")
//  val eunorth1: AwsRegion.Value = Value("eu-north-1")
//  val eucentral1: AwsRegion.Value = Value("eu-central-1")
//  val saeast1: AwsRegion.Value = Value("sa-east-1")
//}