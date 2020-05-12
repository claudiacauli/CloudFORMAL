/*
 *    Copyright 2019 Claudia Cauli
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */

package com.cloud.formal.mapping.templates

import com.cloud.formal.mapping.Specification

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
    "cloudtrail::trail" -> "TrailName",
    "config::configurationrecorder"   -> "Name",
    "config::deliverychannel"         -> "Name",
    "config::configrule"    -> "ConfigRuleName",
    "iam::managedpolicy"    -> "ManagedPolicyName",
    "apigateway::restapi"   -> "Name",
    "apigateway::usageplan" -> "UsagePlanName"

  )

}
