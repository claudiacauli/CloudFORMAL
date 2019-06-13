package aws.cfn.specifications

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
