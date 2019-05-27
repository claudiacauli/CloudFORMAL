package aws.cfn.maps

object PolicyReferencesMap {

  def lookup(k: String): Option[String] = map.get(k)

  private val map = Map(
    "apigatewayrestapi_restapi_policy" -> "resourcebasedpolicy",
    "ec2vpcendpoint_vpcendpoint_policydocument" -> "resourcebasedpolicy",
    "ecrrepository_repository_repositorypolicytext" -> "resourcebasedpolicy",
    "elasticsearchdomain_domain_accesspolicies" -> "resourcebasedpolicy",
    "iamgroup_policy_policydocument" -> "identitybasedpolicy",
    "iammanagedpolicy_managedpolicy_policydocument" -> "identitybasedpolicy",
    "iampolicy_policy_policydocument" -> "identitybasedpolicy",
    "iamrole_role_assumerolepolicydocument" -> "resourcebasedpolicy",
    "iamrole_policy_policydocument" -> "identitybasedpolicy",
    "iamuser_policy_policydocument" -> "identitybasedpolicy",
    "iotpolicy_policy_policydocument" -> "resourcebasedpolicy",
    "kmskey_key_keypolicy" -> "resourcebasedpolicy",
    "s3bucketpolicy_bucketpolicy_policydocument" -> "resourcebasedpolicy",
    "secretsmanagerresourcepolicy_resourcepolicy_resourcepolicy" -> "resourcebasedpolicy",
    "snssubscription_subscription_deliverypolicy" -> "resourcebasedpolicy",
    "snssubscription_subscription_filterpolicy" -> "resourcebasedpolicy",
    "snstopicpolicy_topicpolicy_policydocument" -> "resourcebasedpolicy",
    "sqsqueuepolicy_queuepolicy_policydocument" -> "resourcebasedpolicy",
    "sqsqueue_queue_redrivepolicy" -> "resourcebasedpolicy"
  )


}
