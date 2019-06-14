package com.claudiacauli.www.cloudformal.mapping.templates

//noinspection SpellCheckingInspection
private[templates]
object DefaultsMap {


  private[templates]
  def lookUp(k: String) =
    map.get(k.toLowerCase)


  private[templates]
  def lookUp(k1:String, k2:String) =
    map.get(k1.toLowerCase) match {
      case None     => None
      case Some(m)  => m.get(k2.toLowerCase)
    }





  private val map : Map[String,Map[String,Any]] = Map(

    "applicationautoscaling" -> Map(),

    "autoscalingplans" -> Map(),

    "batch" -> Map(),

    "cloudformation" -> Map(),

    "cloudwatch" ->  Map(),

    "cloudtrail" -> Map("trail_ismultiregiontrail" -> false,
      "trail_enablelogfilevalidation" -> false,
      "trail_includeglobalserviceevents" -> false),

    "codebuild" -> Map(),

    "codecommit" -> Map(),

    "config" -> Map(),

    "dynamodb" -> Map("table_billingmode" -> "provisioned", "ssespecification_sseenabled" -> false,
      "pointintimerecoveryspecification_pointintimerecoveryenabled" -> false),

    "ec2" -> Map(),

    "ecr" -> Map(),

    "ecs" -> Map(),

    "eks" -> Map(),

    "elasticache" -> Map(),

    "elasticbeanstalk" -> Map(),

    "elasticLoadbalancing" -> Map(),

    "iam" -> Map(),

    "kinesis" -> Map(),

    "lambda" -> Map(),

    "logs" -> Map(),

    "rds" -> Map(),

    "s3" -> Map("bucket_accesscontrol" -> "private"),

    "sns" -> Map(),

    "sqs" -> Map(),

    "sts" -> Map()

  )

}
