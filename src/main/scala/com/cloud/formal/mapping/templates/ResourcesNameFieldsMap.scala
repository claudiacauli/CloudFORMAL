package com.claudiacauli.www.cloudformal.mapping.templates

import aws.cfn.mapping.Specification

//noinspection SpellCheckingInspection
private object ResourcesNameFieldsMap {


  def lookUp(s: String, r: String) : Option[String] =
    map.get(s.toLowerCase +
      Specification.TypeDelimiter +
      r.toLowerCase)


  private val map : Map[String,String] = Map (
    // Resource -> Name of top level property containing resource's name

    "s3::bucket"        -> "BucketName",
    "sqs::queue"        -> "QueueName",
    "sns::topic"        -> "TopicName",
    "route53::hostedzone"   -> "Name",
    "route53::recordset"    -> "Name",
    "logs::logstream"     -> "LogStreamName",
    "logs::loggroup"      -> "LogGroupName",
    "logs::destination"   -> "DestinationName",
    "lambda::function"    -> "FunctionName",
    "kinesis::stream"     -> "Name",
    "kms::alias"        -> "AliasName",
    "iam::role"         -> "RoleName",
    "iam::group"        -> "GroupName",
    "events::rule"      -> "Name",
    "dynamodb::table"   -> "TableName",
    "cloudwatch::alarm" -> "AlarmName",
    "config::configurationrecorder"   -> "Name",
    "config::deliverychannel"         -> "Name",
    "config::configrule"    -> "ConfigRuleName",
    "iam::managedpolicy"    -> "ManagedPolicyName",
    "apigateway::restapi"   -> "Name",
    "apigateway::usageplan" -> "UsagePlanName"

  )

}
