package aws.cfn.mapping.templates

//noinspection SpellCheckingInspection
private[templates]
object DefaultsMap {


  private[templates]
  def lookUp(k: String) =
    map.get(k)


  private[templates]
  def lookUp(k1:String, k2:String)
  = map(k1).get(k2)




  private val map : Map[String,Map[String,AnyVal]] = Map(

    "ApplicationAutoScaling" -> Map(),

    "AutoScalingPlans" -> Map(),

    "Batch" -> Map(),

    "CloudFormation" -> Map(),

    "CloudWatch" ->  Map(),

    "CodeBuild" -> Map(),

    "CodeCommit" -> Map(),

    "Config" -> Map(),

    "DynamoDB" -> Map(),

    "Ec2" -> Map(),

    "ECR" -> Map(),

    "ECS" -> Map(),

    "EKS" -> Map(),

    "ElastiCache" -> Map(),

    "ElasticBeanstalk" -> Map(),

    "ElasticLoadBalancing" -> Map(),

    "IAM" -> Map(),

    "Kinesis" -> Map(),

    "Lambda" -> Map(),

    "Logs" -> Map(),

    "RDS" -> Map(),

    "S3" -> Map(),

    "SNS" -> Map(),

    "SQS" -> Map(),

    "STS" -> Map()

  )

}
