package aws.cfn.maps


object ActionsMap {


  // https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_actions-resources-contextkeys.html
  val actionPrefixesFromService : Map[String,Vector[String]] =
    Map(
    "apigateway"              -> Vector("execute-api"),
    "applicationautoscaling"  -> Vector("application-autoscaling"),
    "autoscaling"             -> Vector("autoscaling-plans", "autoscaling"  ),
    "certificatemanager"      -> Vector("acm"),
    "cognito"                 -> Vector("cognito-identity", "cognito-sync", "cognito-idp"),
    "dynamodb"                -> Vector("dynamodb", "dax"),
    "efs"                     -> Vector("elasticfilesystem"),
    "elasticloadbalancingv2"  -> Vector("elasticloadbalancing"),
    "emr"                     -> Vector("elasticmapreduce"),
    "elasticsearch"           -> Vector("es"),
    "kinesisanalyticsv2"      -> Vector("kinesisanalytics"),
    "kinesisfirehose"         -> Vector("firehose"),
    "neptune"                 -> Vector("neptune-db"),
    "opsworkscm"              -> Vector("opsworks-cm"),
    "rds"                     -> Vector("rds","rds-data"),
    "route53"                 -> Vector("route53","route53domains"),
    "stepfunctions"           -> Vector("states"),
    "wafregional"             -> Vector("waf-regional")
  )

  def lookUp(serviceName: String): Vector[(String, Map[String, Vector[String]])] = {

    actionPrefixesFromService.getOrElse(serviceName,Vector(serviceName)) flatMap ( actPrefix =>
      Map(actPrefix -> map.getOrElse(actPrefix,Map()))
    )

  }

  def lookUp(serviceName:String, resourceType:String) : Vector[String] = {
    actionPrefixesFromService.getOrElse(serviceName,Vector(serviceName)) flatMap
      ( actPrefix =>
        map.getOrElse(actPrefix,Map())
        .getOrElse(resourceType,
          map.getOrElse(actPrefix,Map())
            .getOrElse("",Vector())) )
  }

  def getKeys: Iterable[String] = map.keys

  private val map : Map[String,Map[String,Vector[String]]] = Map(

      "acm" -> Map(
        "" -> Vector("DeleteCertificate","DescribeCertificate","RequestCertificate")
      ),

      "apigateway" -> Map(
        "RestApi" -> Vector("Get","Post", "Delete", "Patch")
      ),

      "applicationautoscaling" ->
        Map(
          "ScalingPolicy" ->  Vector("DeleteScalingPolicy", "DescribeScalingPolicy", "PutScalingPolicy"),

          "ScalableTarget" ->  Vector("DeleteScheduledAction", "DeregisterScalableTarget", "DescribeScalableTargets",
              "DescribeScalingActivities", "DescribeScheduledActions", "PutScheduledAction", "RegisterScalableTarget")),



      "autoscalingplans" ->
        Map(
          "ScalingPlan" -> Vector("CreateScalingPlan", "DeleteScalingPlan", "DescribeScalingPlanResources",
              "DescribeScalingPlans", "GetScalingPlanResourceForecastData", "UpdateScalingPlan")),



      "batch" ->
        Map(
          "JobQueue" -> Vector("CancelJob", "CreateJobQueue", "DeleteJobQueue", "DeregisterJobDefinition",
               "DescribeJobDefinitions", "DescribeJobQueues", "DescribeJobs", "ListJobs", "RegisterJobDefinition",
               "SubmitJob", "TerminateJob", "UpdateJobQueue"),

          "ComputeEnvironment" -> Vector("DescribeComputeEnvironments", "UpdateComputeEnvironment")),



      "cloudformation" -> Map(
        "Stack" -> Vector("CancelUpdateStack", "CreateStack", "CreateStackInstances", "CreateStackSet",
          "DeleteStack", "DeleteStackInstances", "DeleteStackSet", "ContinueUpdateRollback", "CreateChangeSet", "DeleteChangeSet",
          "DescribeAccountLimits", "DescribeChangeSet", "DescribeStackDriftDetectionStatus", "DescribeStackEvents",
          "DescribeStackInstance", "DescribeStackResource", "DescribeStackResourceDrifts", "DescribeStackResources",
          "DescribeStacks", "DescribeStackSet", "DescribeStackSetOperation", "DetectStackDrift", "DetectStackResourceDrift",
          "EstimateTemplateCost", "ExecuteChangeSet", "GetStackPolicy", "GetTemplate", "GetTemplateSummary", "ListChangeSets",
          "ListExports", "ListImports", "ListStackInstances", "ListStackResources", "ListStacks", "ListStackSetOperationResults",
          "ListStackSetOperations", "ListStackSets", "SetStackPolicy", "SignalResource", "StopStackSetOperation",
          "UpdateStack", "UpdateStackInstances", "UpdateStackSet", "UpdateTerminationProtection", "ValidateTemplate")),



      "cloudwatch" ->  Map(
        "Alarm" -> Vector("DeleteAlarms", "DescribeAlarmHistory", "DescribeAlarms", "DescribeAlarmsForMetric",
            "DisableAlarmActions", "EnableAlarmActions", "SetAlarmState"),

        "Dashboard" -> Vector("DeleteDashboards", "GetDashboard", "GetMetricData", "GetMetricStatistics",
            "GetMetricWidgetImage", "ListDashboards", "ListMetrics", "ListTagsForResource", "PutDashboard", "PutMetricAlarm",
            "PutMetricData", "TagResource", "UntagResource")),



      "codebuild" -> Map(
        "Project" -> Vector("BatchDeleteBuilds", "BatchGetBuilds", "BatchGetProjects", "CreateProject", "CreateWebhook",
            "DeleteProject", "DeleteSourceCredentials", "DeleteWebhook", "ImportSourceCredentials", "InvalidateProjectCache",
            "ListBuilds", "ListBuildsForProject", "ListCuratedEnvironmentImages", "ListProjects", "ListSourceCredentials",
            "StartBuild", "StopBuild", "UpdateProject", "UpdateWebhook")),


      "codecommit" -> Map(),


      "config" -> Map(),


      "dynamodb" -> Map(
        "Table" -> Vector(
          "BatchGetItem", "BatchWriteItem", "CreateBackup", "CreateGlobalTable", "CreateTable", "DeleteBackup",
          "DeleteItem", "DeleteTable", "DescribeBackup", "DescribeContinuousBackups", "DescribeEndpoints",
          "DescribeGlobalTable", "DescribeGlobalTableSettings", "DescribeLimits", "DescribeTable", "DescribeTimeToLive",
          "GetItem", "ListBackups", "ListGlobalTables", "ListTables", "ListTagsOfResource", "PutItem", "Query",
          "RestoreTableFromBackup", "RestoreTableToPointInTime", "Scan", "TagResource", "TransactGetItems",
          "TransactWriteItems", "UntagResource", "UpdateContinuousBackups", "UpdateGlobalTable", "UpdateGlobalTableSettings",
          "UpdateItem", "UpdateTable", "UpdateTimeToLive", "DescribeStream", "GetRecords", "GetShardIterator",
          "ListStreams"
        )
      ),


      "ec2" -> Map(),


      "ecr" -> Map(),


      "ecs" -> Map(),


      "eks" -> Map(),


      "elasticache" -> Map(),


      "elasticbeanstalk" -> Map(),


      "elasticloadbalancing" -> Map(),


      "iam" -> Map(
        "" -> Vector("ListPolicies", "GetPolicyVersion")
      ),


      "kms" -> Map(
        "Key" -> Vector(
          "CancelKeyDeletion", "ConnectCustomKeyStore", "CreateAlias", "CreateCustomKeyStore", "CreateGrant", "CreateKey",
          "Decrypt", "DeleteAlias", "DeleteCustomKeyStore", "DeleteImportedKeyMaterial", "DescribeCustomKeyStores",
          "DescribeKey", "DisableKey", "DisableKeyRotation", "DisconnectCustomKeyStore", "EnableKey", "EnableKeyRotation",
          "Encrypt", "GenerateDataKey", "GenerateDataKeyWithoutPlaintext", "GenerateRandom", "GetKeyPolicy",
          "GetKeyRotationStatus", "GetParametersForImport", "ImportKeyMaterial", "ListAliases", "ListGrants",
          "ListKeyPolicies", "ListKeys", "ListResourceTags", "ListRetirableGrants", "PutKeyPolicy", "ReEncrypt",
          "RetireGrant", "RevokeGrant", "ScheduleKeyDeletion", "TagResource", "UntagResource", "UpdateAlias",
          "UpdateCustomKeyStore", "UpdateKeyDescription"
        )),


      "kinesis" -> Map(
        "Stream" -> Vector(
          "AddTagsToStream", "CreateStream", "DecreaseStreamRetentionPeriod", "DeleteStream", "DeregisterStreamConsumer",
          "DescribeLimits", "DescribeStream", "DescribeStreamConsumer", "DescribeStreamSummary", "DisableEnhancedMonitoring",
          "EnableEnhancedMonitoring", "GetRecords", "GetShardIterator", "IncreaseStreamRetentionPeriod", "ListShards",
          "ListStreamConsumers", "ListStreams", "ListTagsForStream", "MergeShards", "PutRecord", "PutRecords",
          "RegisterStreamConsumer", "RemoveTagsFromStream", "SplitShard", "StartStreamEncryption", "StopStreamEncryption",
          "SubscribeToShard", "UpdateShardCount"
      )),


      "lambda" -> Map(),


      "logs" -> Map(
        "LogGroup" -> Vector(
          "AssociateKmsKey", "CreateLogGroup", "DeleteLogGroup", "DescribeLogGroups", "GetLogGroupFields",
          "ListTagsLogGroup", "TagLogGroup", "UntagLogGroup"),
        "LogStream" -> Vector("CancelExportTask", "CreateExportTask", "CreateLogStream", "DeleteLogStream" , "DeleteResourcePolicy",
          "DescribeSubscriptionFilters", "DescribeResourcePolicies", "DisassociateKmsKey", "FilterLogEvents",
          "DeleteRetentionPolicy", "DescribeDestinations", "DescribeExportTasks", "DescribeLogStreams", "GetLogEvents",
          "GetLogRecord", "PutDestination", "PutDestinationPolicy", "PutLogEvents", "PutMetricFilter", "PutResourcePolicy",
          "PutRetentionPolicy"),
        "Destination" -> Vector("DeleteDestination"),
        "MetricFilter" -> Vector("DeleteMetricFilter", "DescribeMetricFilters", "DescribeQueries", "GetQueryResults",
        "StartQuery", "StopQuery", "TestMetricFilter"),
        "SubscriptionFilter" -> Vector("DeleteSubscriptionFilter", "PutSubscriptionFilter")
        ),


      "rds" -> Map(),


      "s3" -> Map(
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


      "sns" -> Map(
        "Topic" -> Vector(
          "AddPermission", "CheckIfPhoneNumberIsOptedOut", "ConfirmSubscription", "CreatePlatformApplication", "CreatePlatformEndpoint",
          "CreateTopic", "DeleteEndpoint", "DeletePlatformApplication", "DeleteTopic", "GetEndpointAttributes",
          "GetPlatformApplicationAttributes", "GetSMSAttributes", "GetSubscriptionAttributes", "GetTopicAttributes",
          "ListEndpointsByPlatformApplication", "ListPhoneNumbersOptedOut", "ListPlatformApplications",
          "ListSubscriptions", "ListSubscriptionsByTopic", "ListTagsForResource", "ListTopics", "OptInPhoneNumber",
          "Publish","RemovePermission", "SetEndpointAttributes", "SetPlatformApplicationAttributes", "SetSMSAttributes",
          "SetSubscriptionAttributes", "SetTopicAttributes", "Subscribe", "TagResource", "Unsubscribe",
          "UntagResource"
        )
      ),

      "ses" -> Map(
        "" -> Vector("SendEmail","SendRawEmail")
      ),

      "sqs" -> Map(),


      "sts" -> Map(
        "" -> Vector(
          "AssumeRole", "AssumeRoleWithSAML", "AssumeRoleWithWebIdentity", "DecodeAuthorizationMessage", "GetCallerIdentity",
          "GetFederationToken", "GetSessionToken"
        )
      )



  )

}
