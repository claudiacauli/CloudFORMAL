package aws.cfn.specification.maps

object ActionsMap {

  def lookUp(k: String) : Option[Map[String,Vector[String]]]= map.get(k)
  def lookUp(k1:String, k2:String) : Option[Vector[String]] = map(k1).get(k2)
  def getKeys: Iterable[String] = map.keys

  private val map : Map[String,Map[String,Vector[String]]] = Map(

      "ApplicationAutoScaling" ->
        Map(
          "ScalingPolicy" ->  Vector("DeleteScalingPolicy", "DescribeScalingPolicy", "PutScalingPolicy"),

          "ScalableTarget" ->  Vector("DeleteScheduledAction", "DeregisterScalableTarget", "DescribeScalableTargets",
              "DescribeScalingActivities", "DescribeScheduledActions", "PutScheduledAction", "RegisterScalableTarget")),



      "AutoScalingPlans" ->
        Map(
          "ScalingPlan" -> Vector("CreateScalingPlan", "DeleteScalingPlan", "DescribeScalingPlanResources",
              "DescribeScalingPlans", "GetScalingPlanResourceForecastData", "UpdateScalingPlan")),



      "Batch" ->
        Map(
          "JobQueue" -> Vector("CancelJob", "CreateJobQueue", "DeleteJobQueue", "DeregisterJobDefinition",
               "DescribeJobDefinitions", "DescribeJobQueues", "DescribeJobs", "ListJobs", "RegisterJobDefinition",
               "SubmitJob", "TerminateJob", "UpdateJobQueue"),

          "ComputeEnvironment" -> Vector("DescribeComputeEnvironments", "UpdateComputeEnvironment")),



      "CloudFormation" -> Map(
        "Stack" -> Vector("CancelUpdateStack", "CreateStack", "CreateStackInstances", "CreateStackSet",
          "DeleteStack", "DeleteStackInstances", "DeleteStackSet", "ContinueUpdateRollback", "CreateChangeSet", "DeleteChangeSet",
          "DescribeAccountLimits", "DescribeChangeSet", "DescribeStackDriftDetectionStatus", "DescribeStackEvents",
          "DescribeStackInstance", "DescribeStackResource", "DescribeStackResourceDrifts", "DescribeStackResources",
          "DescribeStacks", "DescribeStackSet", "DescribeStackSetOperation", "DetectStackDrift", "DetectStackResourceDrift",
          "EstimateTemplateCost", "ExecuteChangeSet", "GetStackPolicy", "GetTemplate", "GetTemplateSummary", "ListChangeSets",
          "ListExports", "ListImports", "ListStackInstances", "ListStackResources", "ListStacks", "ListStackSetOperationResults",
          "ListStackSetOperations", "ListStackSets", "SetStackPolicy", "SignalResource", "StopStackSetOperation",
          "UpdateStack", "UpdateStackInstances", "UpdateStackSet", "UpdateTerminationProtection", "ValidateTemplate")),



      "CloudWatch" ->  Map(
        "Alarm" -> Vector("DeleteAlarms", "DescribeAlarmHistory", "DescribeAlarms", "DescribeAlarmsForMetric",
            "DisableAlarmActions", "EnableAlarmActions", "SetAlarmState"),

        "Dashboard" -> Vector("DeleteDashboards", "GetDashboard", "GetMetricData", "GetMetricStatistics",
            "GetMetricWidgetImage", "ListDashboards", "ListMetrics", "ListTagsForResource", "PutDashboard", "PutMetricAlarm",
            "PutMetricData", "TagResource", "UntagResource")),



      "CodeBuild" -> Map(
        "Project" -> Vector("BatchDeleteBuilds", "BatchGetBuilds", "BatchGetProjects", "CreateProject", "CreateWebhook",
            "DeleteProject", "DeleteSourceCredentials", "DeleteWebhook", "ImportSourceCredentials", "InvalidateProjectCache",
            "ListBuilds", "ListBuildsForProject", "ListCuratedEnvironmentImages", "ListProjects", "ListSourceCredentials",
            "StartBuild", "StopBuild", "UpdateProject", "UpdateWebhook")),


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


      "S3" -> Map(
        "Bucket" -> Vector("AbortMultipartUpload", "CreateBucket", "CreateJob", "DeleteBucket", "DeleteBucketWebsite",
            "DeleteObject", "DeleteObjectTagging", "DeleteObjectVersion", "DeleteObjectVersionTagging", "DescribeJob",
            "GetAccelerateConfiguration", "GetAccountPublicAccessBlock", "GetAnalyticsConfiguration", "GetBucketAcl",
            "GetBucketCORS", "GetBucketLocation", "GetBucketLogging", "GetBucketNotification", "GetBucketObjectLockConfiguration",
            "GetBucketPublicAccessBlock", "GetBucketRequestPayment", "GetBucketTagging", "GetBucketVersioning", "GetBucketWebsite",
            "GetEncryptionConfiguration", "GetInventoryConfiguration", "GetLifecycleConfiguration", "GetMetricsConfiguration",
            "GetObject", "GetObjectAcl", "GetObjectLegalHold", "GetObjectRetention", "GetObjectTagging", "GetObjectTorrent",
            "GetObjectVersion", "GetObjectVersionAcl", "GetObjectVersionForReplication", "GetObjectVersionTagging" ,
            "GetReplicationConfiguration", "GetObjectVersionTorrent", "ListAllMyBuckets", "ListBucket", "ListBucketByTags",
            "ListBucketMultipartUploads", "ListBucketVersions", "ListJobs", "ListMultipartUploadParts", "ObjectOwnerOverrideToBucketOwner",
            "PutAccelerateConfiguration", "PutAccountPublicAccessBlock", "PutAnalyticsConfiguration", "PutBucketAcl",
            "PutBucketCORS", "PutBucketLogging", "PutBucketNotification", "PutBucketObjectLockConfiguration",
            "PutBucketPublicAccessBlock", "PutBucketRequestPayment", "PutBucketTagging", "PutBucketVersioning",
            "PutBucketWebsite", "PutEncryptionConfiguration", "PutInventoryConfiguration", "PutLifecycleConfiguration",
            "PutMetricsConfiguration", "PutObject", "PutObjectAcl", "PutObjectLegalHold", "PutObjectRetention",
            "PutObjectTagging", "PutObjectVersionAcl", "PutObjectVersionTagging", "PutReplicationConfiguration",
            "ReplicateDelete", "ReplicateObject", "ReplicateTags", "RestoreObject", "UpdateJobPriority", "UpdateJobStatus"),

        "Policy" -> Vector("DeleteBucketPolicy", "GetBucketPolicy", "GetBucketPolicyStatus", "PutBucketPolicy")),


      "SNS" -> Map(),


      "SQS" -> Map(),


      "STS" -> Map()


  )

}
