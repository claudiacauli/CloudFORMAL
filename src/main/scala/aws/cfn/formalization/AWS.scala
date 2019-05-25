package aws.cfn.formalization

object AwsPartition extends Enumeration {
  val aws = Value("aws")
  val awscn = Value("aws-cn")
  val awsusgov = Value("aws-us-gov")
}

object AwsRegion extends Enumeration {
  val useast1 = Value("us-east-1")
  val useast2 = Value("us-east-2")
  val uswest1 = Value("us-west-1")
  val uswest2 = Value("us-west-2")
  val apeast1 = Value("ap-east-1")
  val apsouth1 = Value("ap-south-1")
  val apnortheast3 = Value("ap-northeast-3")
  val apnortheast2 = Value("ap-northeast-2")
  val apsoutheast1 = Value("ap-southeast-1")
  val apsoutheast2 = Value("ap-southeast-2")
  val apnortheast1 = Value("ap-northeast-1")
  val cacentral1 = Value("ca-central-1")
  val cnnorth1 = Value("cn-north-1")
  val cnnorthwest1 = Value("cn-northwest-1")
  val euwest1 = Value("eu-west-1")
  val euwest2 = Value("eu-west-2")
  val euwest3 = Value("eu-west-3")
  val eunorth1 = Value("eu-north-1")
  val eucentral1 = Value("eu-central-1")
  val saeast1 = Value("sa-east-1")
}

