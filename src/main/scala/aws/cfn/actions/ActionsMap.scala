package aws.cfn.actions

//noinspection SpellCheckingInspection
object ActionsMap {


  def getActionPrefixes: Iterable[String] =
    map.keys


  def getActionPrefixFromService(serviceName: String): Set[String] =
    actionPrefixesFromService
      .getOrElse(serviceName,
        Set(serviceName))


  def lookupServiceName(serviceName: String): Set[String] =
    getActionPrefixFromService(serviceName.toLowerCase)
      .flatMap( aPref =>
        map.getOrElse(aPref,Set())
          .map( aName => aPref+":"+aName )
      )


  def getServiceFromActionPrefix(actionPrefix: String): String =
    actionPrefixesFromService.toVector
      .find(_._2 contains actionPrefix) match {
        case None               => actionPrefix
        case Some((service,_))  => service
      }


  private[actions]
  def lookupActionPrefix(actPrefix: String): Set[String] =
    map.getOrElse(actPrefix.toLowerCase, Set())
      .map ( aName =>
        actPrefix.toLowerCase+":"+aName
      )






  // https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_actions-resources-contextkeys.html

  private val actionPrefixesFromService : Map[String,Set[String]] =
    Map(
    "apigateway"              -> Set("execute-api", "apigateway"),
    "applicationautoscaling"  -> Set("application-autoscaling"),
    "autoscaling"             -> Set("autoscaling-plans", "autoscaling"  ),
    "certificatemanager"      -> Set("acm"),
    "cognito"                 -> Set("cognito-identity", "cognito-sync", "cognito-idp"),
    "dynamodb"                -> Set("dynamodb", "dax"),
    "efs"                     -> Set("elasticfilesystem"),
    "elasticloadbalancingv2"  -> Set("elasticloadbalancing"),
    "emr"                     -> Set("elasticmapreduce"),
    "iam"                     -> Set("iam","sts"),
    "elasticsearch"           -> Set("es"),
    "kinesisanalyticsv2"      -> Set("kinesisanalytics"),
    "kinesisfirehose"         -> Set("firehose"),
    "neptune"                 -> Set("neptune-db"),
    "opsworkscm"              -> Set("opsworks-cm"),
    "rds"                     -> Set("rds","rds-data"),
    "route53"                 -> Set("route53","route53domains"),
    "stepfunctions"           -> Set("states"),
    "wafregional"             -> Set("waf-regional")
  )



  private val map : Map[String,Set[String]] = Map(

      "acm"
        -> Set("DeleteCertificate","DescribeCertificate","RequestCertificate"),

      "apigateway"
        -> Set("Get","Post", "Delete", "Patch", "Put"),

      "autoscaling" -> Set(),

      "application-autoscaling"
        -> Set("DeleteScalingPolicy", "DescribeScalingPolicy", "PutScalingPolicy",
        "DeleteScheduledAction", "DeregisterScalableTarget", "DescribeScalableTargets",
        "DescribeScalingActivities", "DescribeScheduledActions", "PutScheduledAction",
        "RegisterScalableTarget"),

      "autoscaling-plans"
        -> Set("CreateScalingPlan", "DeleteScalingPlan", "DescribeScalingPlanResources",
        "DescribeScalingPlans", "GetScalingPlanResourceForecastData",
        "UpdateScalingPlan"),

      "batch"
        -> Set("CancelJob", "CreateJobQueue", "DeleteJobQueue", "DeregisterJobDefinition",
        "DescribeJobDefinitions", "DescribeJobQueues", "DescribeJobs",
        "ListJobs", "RegisterJobDefinition", "SubmitJob", "TerminateJob",
        "UpdateJobQueue", "DescribeComputeEnvironments",
        "UpdateComputeEnvironment"),

      "cloudformation"
        -> Set("CancelUpdateStack", "CreateStack", "CreateStackInstances",
        "CreateStackSet", "DeleteStack", "DeleteStackInstances",
        "DeleteStackSet", "ContinueUpdateRollback", "CreateChangeSet",
        "DeleteChangeSet", "DescribeAccountLimits", "DescribeChangeSet",
        "DescribeStackDriftDetectionStatus", "DescribeStackEvents",
        "DescribeStackInstance", "DescribeStackResource",
        "DescribeStackResourceDrifts", "DescribeStackResources", "DescribeStacks",
        "DescribeStackSet", "DescribeStackSetOperation",
        "DetectStackDrift", "DetectStackResourceDrift", "EstimateTemplateCost",
        "ExecuteChangeSet", "GetStackPolicy", "GetTemplate", "GetTemplateSummary",
        "ListChangeSets", "ListExports", "ListImports", "ListStackInstances",
        "ListStackResources", "ListStacks", "ListStackSetOperationResults",
        "ListStackSetOperations", "ListStackSets", "SetStackPolicy",
        "SignalResource", "StopStackSetOperation", "UpdateStack",
        "UpdateStackInstances", "UpdateStackSet", "UpdateTerminationProtection",
        "ValidateTemplate"),

      "cloudwatch"
        -> Set("DeleteAlarms", "DescribeAlarmHistory", "DescribeAlarms",
        "DescribeAlarmsForMetric", "DisableAlarmActions", "EnableAlarmActions",
        "SetAlarmState", "DeleteDashboards", "GetDashboard", "GetMetricData",
        "GetMetricStatistics", "GetMetricWidgetImage", "ListDashboards",
        "ListMetrics", "ListTagsForResource", "PutDashboard", "PutMetricAlarm",
        "PutMetricData", "TagResource", "UntagResource"),

      "codebuild"
        -> Set("BatchDeleteBuilds", "BatchGetBuilds", "BatchGetProjects",
        "CreateProject", "CreateWebhook", "DeleteProject",
        "DeleteSourceCredentials", "DeleteWebhook", "ImportSourceCredentials",
        "InvalidateProjectCache", "ListBuilds", "ListBuildsForProject",
        "ListCuratedEnvironmentImages", "ListProjects", "ListSourceCredentials",
        "StartBuild", "StopBuild", "UpdateProject", "UpdateWebhook"),

      "codecommit"
        -> Set(),

      "cognito-identity" -> Set(),

      "cognito-sync" -> Set(),

      "cognito-idp" -> Set(),

      "config"
        -> Set(),

      "dax" -> Set(),

      "dynamodb"
        -> Set("BatchGetItem", "BatchWriteItem", "CreateBackup",
        "CreateGlobalTable", "CreateTable", "DeleteBackup",
        "DeleteItem", "DeleteTable", "DescribeBackup",
        "DescribeContinuousBackups", "DescribeEndpoints", "DescribeGlobalTable",
        "DescribeGlobalTableSettings", "DescribeLimits", "DescribeTable",
        "DescribeTimeToLive", "GetItem", "ListBackups", "ListGlobalTables",
        "ListTables", "ListTagsOfResource", "PutItem", "Query",
        "RestoreTableFromBackup", "RestoreTableToPointInTime", "Scan",
        "TagResource", "TransactGetItems", "TransactWriteItems", "UntagResource",
        "UpdateContinuousBackups", "UpdateGlobalTable", "UpdateGlobalTableSettings",
        "UpdateItem", "UpdateTable", "UpdateTimeToLive", "DescribeStream",
        "GetRecords", "GetShardIterator", "ListStreams"),


      "ec2" -> Set(),


      "ecr" -> Set(),


      "ecs" -> Set(),


      "eks" -> Set(),


      "elasticache" -> Set(),

      "elasticbeanstalk" -> Set(),

      "elasticfilesystem" -> Set(),

      "elasticloadbalancing" -> Set(),

      "elasticmapreduce" -> Set(),

      "es" -> Set(),

      "events" -> Set(),

      "execute-api" -> Set(),

      "firehose" -> Set(),

      "iam"
        -> Set("ListPolicies", "GetPolicyVersion"),

      "kms"
        -> Set("CancelKeyDeletion", "ConnectCustomKeyStore", "CreateAlias",
        "CreateCustomKeyStore", "CreateGrant", "CreateKey", "Decrypt",
        "DeleteAlias", "DeleteCustomKeyStore", "DeleteImportedKeyMaterial",
        "DescribeCustomKeyStores", "DescribeKey", "DisableKey",
        "DisableKeyRotation", "DisconnectCustomKeyStore", "EnableKey",
        "EnableKeyRotation", "Encrypt", "GenerateDataKey",
        "GenerateDataKeyWithoutPlaintext", "GenerateRandom", "GetKeyPolicy",
        "GetKeyRotationStatus", "GetParametersForImport", "ImportKeyMaterial",
        "ListAliases", "ListGrants", "ListKeyPolicies", "ListKeys",
        "ListResourceTags", "ListRetirableGrants", "PutKeyPolicy", "ReEncrypt",
        "RetireGrant", "RevokeGrant", "ScheduleKeyDeletion",
        "TagResource", "UntagResource", "UpdateAlias",
        "UpdateCustomKeyStore", "UpdateKeyDescription"),


      "kinesis"
        -> Set("AddTagsToStream", "CreateStream", "DecreaseStreamRetentionPeriod",
        "DeleteStream", "DeregisterStreamConsumer", "DescribeLimits",
        "DescribeStream", "DescribeStreamConsumer", "DescribeStreamSummary",
        "DisableEnhancedMonitoring", "EnableEnhancedMonitoring", "GetRecords",
        "GetShardIterator", "IncreaseStreamRetentionPeriod", "ListShards",
        "ListStreamConsumers", "ListStreams", "ListTagsForStream",
        "MergeShards", "PutRecord", "PutRecords", "RegisterStreamConsumer",
        "RemoveTagsFromStream", "SplitShard", "StartStreamEncryption",
        "StopStreamEncryption", "SubscribeToShard", "UpdateShardCount"),

      "kinesisanalytics" -> Set(),


      "lambda" -> Set(),


      "logs"
        -> Set("AssociateKmsKey", "CreateLogGroup", "DeleteLogGroup",
        "DescribeLogGroups", "GetLogGroupFields", "ListTagsLogGroup",
        "TagLogGroup", "UntagLogGroup", "CancelExportTask", "CreateExportTask",
        "CreateLogStream", "DeleteLogStream" , "DeleteResourcePolicy",
        "DescribeSubscriptionFilters", "DescribeResourcePolicies",
        "DisassociateKmsKey", "FilterLogEvents", "DeleteRetentionPolicy",
        "DescribeDestinations", "DescribeExportTasks", "DescribeLogStreams",
        "GetLogEvents", "GetLogRecord", "PutDestination", "PutDestinationPolicy",
        "PutLogEvents", "PutMetricFilter", "PutResourcePolicy", "PutRetentionPolicy",
        "DeleteDestination","DeleteMetricFilter", "DescribeMetricFilters",
        "DescribeQueries", "GetQueryResults", "StopQuery", "TestMetricFilter",
        "DeleteSubscriptionFilter", "PutSubscriptionFilter"),

      "neptune-db" -> Set(),

      "opsworks-cm" -> Set(),

      "rds" -> Set(),

      "rds-data" -> Set(),

      "route53" -> Set(),

      "route53domains" -> Set(),

      "s3"
        -> Set("AbortMultipartUpload", "CreateBucket", "CreateJob",
        "DeleteBucket", "DeleteBucketWebsite", "DeleteObject",
        "DeleteObjectTagging", "DeleteObjectVersion", "DeleteObjectVersionTagging",
        "DescribeJob", "GetAccelerateConfiguration", "GetAccountPublicAccessBlock",
        "GetAnalyticsConfiguration", "GetBucketAcl", "GetBucketCORS",
        "GetBucketLocation", "GetBucketLogging", "GetBucketNotification",
        "GetBucketObjectLockConfiguration", "GetBucketPublicAccessBlock",
        "GetBucketRequestPayment", "GetBucketTagging", "GetBucketVersioning",
        "GetBucketWebsite", "GetEncryptionConfiguration", "GetInventoryConfiguration",
        "GetLifecycleConfiguration", "GetMetricsConfiguration", "GetObject",
        "GetObjectAcl", "GetObjectLegalHold", "GetObjectRetention",
        "GetObjectTagging", "GetObjectTorrent", "GetObjectVersion",
        "GetObjectVersionAcl", "GetObjectVersionForReplication", "GetObjectVersionTagging" ,
        "GetReplicationConfiguration", "GetObjectVersionTorrent", "ListAllMyBuckets",
        "ListBucket", "ListBucketByTags", "ListBucketMultipartUploads",
        "ListBucketVersions", "ListJobs", "ListMultipartUploadParts",
        "ObjectOwnerOverrideToBucketOwner", "PutAccelerateConfiguration",
        "PutAccountPublicAccessBlock", "PutAnalyticsConfiguration", "PutBucketAcl",
        "PutBucketCORS", "PutBucketLogging", "PutBucketNotification",
        "PutBucketObjectLockConfiguration", "PutBucketPublicAccessBlock",
        "PutBucketRequestPayment", "PutBucketTagging", "PutBucketVersioning",
        "PutBucketWebsite", "PutEncryptionConfiguration",
        "PutInventoryConfiguration", "PutLifecycleConfiguration",
        "PutMetricsConfiguration", "PutObject", "PutObjectAcl",
        "PutObjectLegalHold", "PutObjectRetention", "PutObjectTagging",
        "PutObjectVersionAcl", "PutObjectVersionTagging", "PutReplicationConfiguration",
        "ReplicateDelete", "ReplicateObject", "ReplicateTags",
        "RestoreObject", "UpdateJobPriority", "UpdateJobStatus",
        "DeleteBucketPolicy", "GetBucketPolicy", "GetBucketPolicyStatus",
        "PutBucketPolicy"),


      "sns"
        -> Set(
          "AddPermission", "CheckIfPhoneNumberIsOptedOut",
        "ConfirmSubscription", "CreatePlatformApplication",
        "CreatePlatformEndpoint", "CreateTopic", "DeleteEndpoint",
        "DeletePlatformApplication", "DeleteTopic", "GetEndpointAttributes",
        "GetPlatformApplicationAttributes", "GetSMSAttributes",
        "GetSubscriptionAttributes", "GetTopicAttributes",
        "ListEndpointsByPlatformApplication", "ListPhoneNumbersOptedOut",
        "ListPlatformApplications", "ListSubscriptions",
        "ListSubscriptionsByTopic", "ListTagsForResource",
        "ListTopics", "OptInPhoneNumber", "Publish","RemovePermission",
        "SetEndpointAttributes", "SetPlatformApplicationAttributes",
        "SetSMSAttributes", "SetSubscriptionAttributes",
        "SetTopicAttributes", "Subscribe", "TagResource", "Unsubscribe",
        "UntagResource"),

      "ses"
        -> Set("SendEmail","SendRawEmail"),

      "sqs" -> Set(),

      "states" -> Set(),

      "sts"
        -> Set(
          "AssumeRole", "AssumeRoleWithSAML", "AssumeRoleWithWebIdentity",
        "DecodeAuthorizationMessage", "GetCallerIdentity", "GetFederationToken",
        "GetSessionToken"),

      "waf-regional" -> Set()

  )

}