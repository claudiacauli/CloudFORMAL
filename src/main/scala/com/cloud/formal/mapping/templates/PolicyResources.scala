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

object PolicyResources {


  private[templates]
  def lookupServResName(service: String, resource: String) =
    policiesMap(
      (service+Specification.TypeDelimiter+resource)
        .toLowerCase
    )


  private[templates]
  def containsResource(service: String, resource: String) =
    policiesMap.keySet
      .contains(
        (service+Specification.TypeDelimiter+resource)
          .toLowerCase
      )


  private val policiesMap =
    Map(
      "s3::bucketpolicy"
        -> Vector("Bucket"),
      "iam::managedpolicy"
        -> Vector("Users","Groups","Roles"),
      "iam::policy"
        -> Vector("Users","Groups","Roles"),
      "iot::policy"
        -> Vector(),
      "sqs::queuepolicy"
        -> Vector("Queues"),
      "sns::topicpolicy"
        -> Vector("Topics"),
      "secretsmanager::resourcepolicy"
        -> Vector("SecretId")
    )

}

