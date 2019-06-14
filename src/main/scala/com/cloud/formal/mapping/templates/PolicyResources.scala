package com.claudiacauli.www.cloudformal.mapping.templates

import aws.cfn.mapping.Specification

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

