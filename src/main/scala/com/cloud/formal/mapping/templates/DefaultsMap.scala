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

    "cloudwatch" ->  Map("alarm_actionsenabled" -> true),

    "cloudtrail" -> Map("trail_ismultiregiontrail" -> false,
      "trail_enablelogfilevalidation" -> false,
      "trail_includeglobalserviceevents" -> false),

    "codebuild" -> Map(),

    "codecommit" -> Map(),

    "config" -> Map("recordinggroup_allsupported" -> true),

    "dynamodb" -> Map("table_billingmode" -> "provisioned", "ssespecification_sseenabled" -> false,
      "pointintimerecoveryspecification_pointintimerecoveryenabled" -> false),

    "ec2" -> Map("networkinterface_associatepublicipaddress" -> true, "networkaclentry_egress" -> false),

    "ecr" -> Map(),

    "ecs" -> Map(),

    "eks" -> Map(),

    "elasticache" -> Map(),

    "elasticbeanstalk" -> Map(),

    "elasticLoadbalancing" -> Map(),

    "iam" -> Map("role_maxsessionduration" -> 3600),

    "kinesis" -> Map(),

    "kms" -> Map("key_enabled" -> true, "key_enablekeyrotation" -> false, "key_keyusage" -> "ENCRYPT_DECRYPT",
    "key_pendingwindowindays" -> 30),

    "lambda" -> Map(),

    "logs" -> Map(),

    "redshift" -> Map("cluster_encrypted" -> false, "cluster_allowversionupgrade" -> true),

    "rds" -> Map("dbinstance_backupretentionperiod" -> 1, "dbinstance_autominorversionupgrade" -> true,
    "dbinstance_storageencrypted" -> false),

    "s3" -> Map("bucket_accesscontrol" -> "private"),

    "sns" -> Map(),

    "sqs" -> Map(),

    "sts" -> Map()

  )

}
