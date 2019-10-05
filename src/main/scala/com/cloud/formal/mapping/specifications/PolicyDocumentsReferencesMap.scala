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

package com.cloud.formal.mapping.specifications

//noinspection SpellCheckingInspection
private object PolicyDocumentsReferencesMap {

  private[specifications] def lookup(k: String): Option[String] = map.get(k)

  private val map = Map(

    // Full Property Name -> Type of Policy Document (JSON)
    // This field contains a json: cannot also be an inter-resource reference

    "ec2vpcendpoint_vpcendpoint_policydocument"
      -> "resourcebasedpolicy",

    "ecrrepository_repository_repositorypolicytext"
      -> "resourcebasedpolicy",

    "iamgroup_policy_policydocument"
      -> "identitybasedpolicy",

    "iammanagedpolicy_managedpolicy_policydocument"
      -> "identitybasedpolicy",

    "iampolicy_policy_policydocument"
      -> "identitybasedpolicy",

    "iamrole_role_assumerolepolicydocument"
      -> "assumerolepolicy",
    "iamrole_policy_policydocument"
      -> "identitybasedpolicy",

    "iamuser_policy_policydocument"
      -> "identitybasedpolicy",

    "iotpolicy_policy_policydocument"
      -> "resourcebasedpolicy",

    "snstopicpolicy_topicpolicy_policydocument"
      -> "resourcebasedpolicy",

    "sqsqueuepolicy_queuepolicy_policydocument"
      -> "resourcebasedpolicy",

    "s3bucketpolicy_bucketpolicy_policydocument"
      -> "resourcebasedpolicy",

    "secretsmanagerresourcepolicy_resourcepolicy_resourcepolicy"
      -> "resourcebasedpolicy",

    "snssubscription_subscription_deliverypolicy"
      -> "resourcebasedpolicy",
    "snssubscription_subscription_filterpolicy"
      -> "resourcebasedpolicy",

    "kmskey_key_keypolicy"
      -> "resourcebasedpolicy",

    "elasticsearchdomain_domain_accesspolicies"
      -> "resourcebasedpolicy",

    "apigatewayrestapi_restapi_policy"
      -> "resourcebasedpolicy",

    "sqsqueue_queue_redrivepolicy"
      -> "resourcebasedpolicy"

  )


}
