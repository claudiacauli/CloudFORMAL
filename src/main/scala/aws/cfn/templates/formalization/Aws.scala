package aws.cfn.templates.formalization

object AwsPartition extends Enumeration {
  val aws: AwsPartition.Value = Value("aws")
  val awscn: AwsPartition.Value = Value("aws-cn")
  val awsusgov: AwsPartition.Value = Value("aws-us-gov")
}

object AwsRegion extends Enumeration {
  val useast1: AwsRegion.Value = Value("us-east-1")
  val useast2: AwsRegion.Value = Value("us-east-2")
  val uswest1: AwsRegion.Value = Value("us-west-1")
  val uswest2: AwsRegion.Value = Value("us-west-2")
  val apeast1: AwsRegion.Value = Value("ap-east-1")
  val apsouth1: AwsRegion.Value = Value("ap-south-1")
  val apnortheast3: AwsRegion.Value = Value("ap-northeast-3")
  val apnortheast2: AwsRegion.Value = Value("ap-northeast-2")
  val apsoutheast1: AwsRegion.Value = Value("ap-southeast-1")
  val apsoutheast2: AwsRegion.Value = Value("ap-southeast-2")
  val apnortheast1: AwsRegion.Value = Value("ap-northeast-1")
  val cacentral1: AwsRegion.Value = Value("ca-central-1")
  val cnnorth1: AwsRegion.Value = Value("cn-north-1")
  val cnnorthwest1: AwsRegion.Value = Value("cn-northwest-1")
  val euwest1: AwsRegion.Value = Value("eu-west-1")
  val euwest2: AwsRegion.Value = Value("eu-west-2")
  val euwest3: AwsRegion.Value = Value("eu-west-3")
  val eunorth1: AwsRegion.Value = Value("eu-north-1")
  val eucentral1: AwsRegion.Value = Value("eu-central-1")
  val saeast1: AwsRegion.Value = Value("sa-east-1")
}

