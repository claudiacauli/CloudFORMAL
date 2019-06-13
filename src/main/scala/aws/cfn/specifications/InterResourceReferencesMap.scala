package aws.cfn.specifications

//noinspection SpellCheckingInspection,SpellCheckingInspection
private object InterResourceReferencesMap{

  private[specifications] def lookUp(k: String) : Option[(String,String,Boolean,Boolean)]= map.get(k)
                                                                                       // req, fun
  private val map = Map(

    "ec2egressonlyinternetgateway_egressonlyinternetgateway_vpcid"
      -> ("ec2vpc", "vpc", true, true ),

    "ec2eip_eip_instanceid"
      -> ("ec2instance", "instance", false, true ),

    "ec2eipassociation_eipassociation_eip"
      -> ("ec2eip", "eip", false, true ),
    "ec2eipassociation_eipassociation_instanceid"
      -> ("ec2instance", "instance", false, true ),
    "ec2eipassociation_eipassociation_networkinterfaceid"
      -> ("ec2networkinterface", "networkinterface", false, true ),

    "ec2flowlog_flowlog_deliverlogspermissionarn"
      -> ("iamrole", "role", false, true ),
    "ec2flowlog_flowlog_logdestination"
      -> ("null", "null", false, true ),
    "ec2flowlog_flowlog_loggroupname"
      -> ("logsloggroup", "loggroup", false, true ),
    "ec2flowlog_flowlog_resourceid"
      -> ("null", "null", true, true ),

    "ec2instance_instance_hostid"
      -> ("ec2host", "host", false, true ),
    "ec2instance_instance_iaminstanceprofile"
      -> ("iaminstanceprofile", "instanceprofile", false, true ),
    "ec2instance_instance_placementgroupname"
      -> ("ec2placementgroup", "placementgroup", false, true ),
    "ec2instance_instance_securitygroupids"
      -> ("null", "null", false, false ),
    "ec2instance_instance_securitygroups"
      -> ("ec2securitygroup", "securitygroup", false, false ),
    "ec2instance_instance_subnetid"
      -> ("ec2subnet", "subnet", false, true ),
    "ec2instance_launchtemplatespecification_launchtemplateid"
      -> ("ec2launchtemplate", "launchtemplate", false, true ),
    "ec2instance_volume_volumeid"
      -> ("ec2volume", "volume", true, true ),
    "ec2instance_networkinterface_groupset"
      -> ("ec2securitygroup", "securitygroup", false, true ),
    "ec2instance_networkinterface_networkinterfaceid"
      -> ("ec2networkinterface", "networkinterface", false, true ),
    "ec2instance_networkinterface_subnetid"
      -> ("ec2subnet", "subnet", false, true ),

    "ec2launchtemplate_ebs_kmskeyid"
      -> ("kmskey", "key", false, true ),
    "ec2launchtemplate_iaminstanceprofile_arn"
      -> ("iaminstanceprofile", "instanceprofile", false, true ),
    "ec2launchtemplate_networkinterface_groups"
      -> ("ec2securitygroup", "securitygroup", false, false ),
    "ec2launchtemplate_networkinterface_subnetid"
      -> ("ec2subnet", "subnet", false, true ),

    "ec2natgateway_natgateway_allocationid"
      -> ("ec2eip", "eip", true, true ),
    "ec2natgateway_natgateway_subnetid"
      -> ("ec2subnet", "subnet", true, true ),

    "ec2networkacl_networkacl_vpcid"
      -> ("ec2vpc", "vpc", true, true ),

    "ec2networkinterface_networkinterface_groupset"
      -> ("ec2securitygroup", "securitygroup", false, false ),
    "ec2networkinterface_networkinterface_subnetid"
      -> ("ec2subnet", "subnet", false, false ),

    "ec2networkinterfaceattachment_networkinterfaceattachment_instanceid"
      -> ("ec2instance", "instance", true, true ),
    "ec2networkinterfaceattachment_networkinterfaceattachment_networkinterfaceid"
      -> ("ec2networkinterface", "networkinterface", true, true ),

    "ec2networkinterfacepermission_networkinterfacepermission_networkinterfaceid"
      -> ("ec2networkinterface", "networkinterface", true, true ),

    "ec2route_route_egressonlyinternetgatewayid"
      -> ("ec2egressonlyinternetgateway", "egressonlyinternetgateway", false, true ),
    "ec2route_route_gatewayid"
      -> ("null", "null", false, true ),
    "ec2route_route_instanceid"
      -> ("null", "null", false, true ),
    "ec2route_route_natgatewayid"
      -> ("ec2natgateway", "natgateway", false, true ),
    "ec2route_route_networkinterfaceid"
      -> ("null", "null", false, true ),
    "ec2route_route_routetableid"
      -> ("ec2routetable", "routetable", true, true ),
    "ec2route_route_vpcpeeringconnectionid"
      -> ("ec2vpcpeeringconnection", "vpcpeeringconnection", false, true ),

    "ec2routetable_routetable_vpcid"
      -> ("ec2vpc", "vpc", true, true ),

    "ec2securitygroup_securitygroup_vpcid"
      -> ("ec2vpc", "vpc", true, true ),
    "ec2securitygroup_ingress_sourcesecuritygroupid"
      -> ("ec2securitygroup", "securitygroup", true, true ),
    "ec2securitygroup_ingress_sourcesecuritygroupname"
      -> ("ec2securitygroup", "securitygroup", true, true ),
    "ec2securitygroup_egress_destinationsecuritygroupid"
      -> ("ec2securitygroup", "securitygroup", true, true ),
    "ec2securitygroupegress_securitygroupegress_destinationsecuritygroupid"
      -> ("ec2securitygroup", "securitygroup", true, true ),
    "ec2securitygroupingress_securitygroupingress_sourcesecuritygroupname"
      -> ("ec2securitygroup", "securitygroup", true, true ),
    "ec2securitygroupingress_securitygroupingress_sourcesecuritygroupid"
      -> ("ec2securitygroup", "securitygroup", true, true ),

    "ec2volume_volume_kmskeyid"
      -> ("kmskey", "key", false, true ),

    "ec2spotfleet_spotfleetlaunchspecification_subnetid"
      -> ("ec2subnet", "subnet", false, true ),
    "ec2spotfleet_fleetlaunchtemplatespecification_launchtemplateid"
      -> ("ec2launchtemplate", "launchtemplate", false, true ),
    "ec2spotfleet_iaminstanceprofilespecification_arn"
      -> ("iaminstanceprofile", "instanceprofile", false, true ),
    "ec2spotfleet_launchtemplateoverrides_subnetid"
      -> ("ec2subnet", "subnet", false, true ),
    "ec2spotfleet_instancenetworkinterfacespecification_groups"
      -> ("ec2securitygroup", "securitygroup", false, false ),
    "ec2spotfleet_instancenetworkinterfacespecification_subnetid"
      -> ("ec2subnet", "subnet", false, true ),
    "ec2spotfleet_groupidentifier_groupid"
      -> ("ec2securitygroup", "securitygroup", false, true ),
    "ec2spotfleet_spotfleetrequestconfigdata_iamfleetrole"
      -> ("iamrole", "role", false, true ),
    "ec2spotfleet_targetgroup_arn"
      -> ("null", "null", false, true ),

    "ec2subnet_subnet_vpcid"
      -> ("ec2vpc", "vpc", true, true ),

    "ec2subnetcidrblock_subnetcidrblock_subnetid"
      -> ("ec2subnet", "subnet", true, true ),

    "ec2subnetnetworkaclassociation_subnetnetworkaclassociation_subnetid"
      -> ("ec2subnet", "subnet", true, true ),
    "ec2subnetnetworkaclassociation_subnetnetworkaclassociation_networkaclid"
      -> ("ec2networkacl", "networkacl", true, true ),

    "ec2subnetroutetableassociation_subnetroutetableassociation_routetableid"
      -> ("ec2routetable", "routetable", true, true ),
    "ec2subnetroutetableassociation_subnetroutetableassociation_subnetid"
      -> ("ec2subnet", "subnet", true, true ),

    "ec2volumeattachment_volumeattachment_instanceid"
      -> ("ec2instance", "instance", true, true ),
    "ec2volumeattachment_volumeattachment_volumeid"
      -> ("ec2volume", "volume", true, true ),

    "ec2vpccidrblock_vpccidrblock_vpcid"
      -> ("ec2vpc", "vpc", true, true ),

    "ec2vpcdhcpoptionsassociation_vpcdhcpoptionsassociation_dhcpoptionsid"
      -> ("ec2dhcpoptions", "dhcpoptions", true, true ),
    "ec2vpcdhcpoptionsassociation_vpcdhcpoptionsassociation_vpcid"
      -> ("ec2vpc", "vpc", true, true ),

    "ec2vpcendpoint_vpcendpoint_routetableids"
      -> ("ec2routetable", "routetable", false, false ),
    "ec2vpcendpoint_vpcendpoint_securitygroupids"
      -> ("ec2securitygroup", "securitygroup", false, false ),
    "ec2vpcendpoint_vpcendpoint_subnetids"
      -> ("ec2subnet", "subnet", false, false ),
    "ec2vpcendpoint_vpcendpoint_vpcid"
      -> ("ec2vpc", "vpc", true, true ),

    "ec2vpcendpointconnectionnotification_vpcendpointconnectionnotification_connectionnotificationarn"
      -> ("snstopic", "topic", true, true ),
    "ec2vpcendpointconnectionnotification_vpcendpointconnectionnotification_serviceid"
      -> ("ec2vpcendpointservice", "vpcendpointservice", false, true ),
    "ec2vpcendpointconnectionnotification_vpcendpointconnectionnotification_vpcendpointid"
      -> ("ec2vpcendpoint", "vpcendpoint", false, true ),

    "ec2vpcendpointservicepermissions_vpcendpointservicepermissions_allowedprincipals"
      -> ("null", "null", false, false ),
    "ec2vpcendpointservicepermissions_vpcendpointservicepermissions_serviceid"
      -> ("ec2vpcendpointservice", "vpcendpointservice", true, true ),

    "ec2vpcgatewayattachment_vpcgatewayattachment_internetgatewayid"
      -> ("ec2internetgateway", "internetgateway", false, true ),
    "ec2vpcgatewayattachment_vpcgatewayattachment_vpcid"
      -> ("ec2vpc", "vpc", true, true ),
    "ec2vpcgatewayattachment_vpcgatewayattachment_vpngatewayid"
      -> ("ec2vpngateway", "vpngateway", false, true ),
    "ec2vpcpeeringconnection_vpcpeeringconnection_peervpcid"
      -> ("ec2vpc", "vpc", true, true ),
    "ec2vpcpeeringconnection_vpcpeeringconnection_vpcid"
      -> ("ec2vpc", "vpc", true, true ),
    "ec2vpcpeeringconnection_vpcpeeringconnection_peerrolearn"
      -> ("iamrole", "role", false, true ),

    "ec2vpnconnection_vpnconnection_customergatewayid"
      -> ("null", "null", true, true ),
    "ec2vpnconnection_vpnconnection_vpngatewayid"
      -> ("ec2vpngateway", "vpngateway", true, true ),

    "ec2vpnconnectionroute_vpnconnectionroute_vpnconnectionid"
      -> ("ec2vpnconnection", "vpnconnection", true, true ),

    "ec2vpngatewayroutepropagation_vpngatewayroutepropagation_routetableids"
      -> ("ec2routetable", "routetable", true, false ),
    "ec2vpngatewayroutepropagation_vpngatewayroutepropagation_vpngatewayid"
      -> ("ec2vpngateway", "vpngateway", true, true ),

    "ec2transitgatewayattachment_transitgatewayattachment_subnetids"
      -> ("ec2subnet", "subnet", true, false ),
    "ec2transitgatewayattachment_transitgatewayattachment_transitgatewayid"
      -> ("ec2transitgateway", "transitgateway", true, true ),
    "ec2transitgatewayattachment_transitgatewayattachment_vpcid"
      -> ("ec2vpc", "vpc", true, true ),

    "ec2transitgatewayroute_transitgatewayroute_transitgatewayattachmentid"
      -> ("ec2transitgatewayattachment", "transitgatewayattachment", false, true ),
    "ec2transitgatewayroute_transitgatewayroute_transitgatewayroutetableid"
      -> ("ec2transitgatewayroutetable", "transitgatewayroutetable", true, true ),

    "ec2transitgatewayroutetable_transitgatewayroutetable_transitgatewayid"
      -> ("ec2transitgateway", "transitgateway", true, true ),

    "ec2transitgatewayroutetableassociation_transitgatewayroutetableassociation_transitgatewayattachmentid"
      -> ("ec2transitgatewayattachment", "transitgatewayattachment", true, true ),
    "ec2transitgatewayroutetableassociation_transitgatewayroutetableassociation_transitgatewayroutetableid"
      -> ("ec2transitgatewayroutetable", "transitgatewayroutetable", true, true ),

    "ec2transitgatewayroutetablepropagation_transitgatewayroutetablepropagation_transitgatewayattachmentid"
      -> ("ec2transitgatewayattachment", "transitgatewayattachment", true, true ),
    "ec2transitgatewayroutetablepropagation_transitgatewayroutetablepropagation_transitgatewayroutetableid"
      -> ("ec2transitgatewayroutetable", "transitgatewayroutetable", true, true ),

    "iamgroup_group_managedpolicyarns"
      -> ("iammanagedpolicy", "managedpolicy", false, false ),

    "iamuser_user_groups"
      -> ("iamgroup", "group", false, false ),
    "iamuser_user_managedpolicyarns"
      -> ("iammanagedpolicy", "managedpolicy", false, false ),
    "iamuser_user_permissionsboundary"
      -> ("iampolicy", "policy", false, true ),

    "iamusertogroupaddition_usertogroupaddition_groupname"
      -> ("iamgroup", "group", true, true ),
    "iamusertogroupaddition_usertogroupaddition_users"
      -> ("iamuser", "user", true, false ),

    "iamaccesskey_accesskey_username"
      -> ("iamuser", "user", true, true ),

    "iamrole_role_managedpolicyarns"
      -> ("iammanagedpolicy", "managedpolicy", false, false),
    "iamrole_role_permissionsboundary"
      -> ("iampolicy", "policy", false, true ),
    "iamrole_role_policies"
      -> ("iampolicy", "policy", false, false ),

    "iaminstanceprofile_instanceprofile_roles"
      -> ("iamrole", "role", true, false ),

    "iammanagedpolicy_managedpolicy_groups"
      -> ("iamgroup", "group", false, false ),
    "iammanagedpolicy_managedpolicy_roles"
      -> ("iamrole", "role", false, false ),
    "iammanagedpolicy_managedpolicy_users"
      -> ("iamuser", "user", false, false ),

    "iampolicy_policy_groups"
      -> ("iamgroup", "group", false, false ),
    "iampolicy_policy_roles"
      -> ("iamrole", "role", false, false ),
    "iampolicy_policy_users"
      -> ("iamuser", "user", false, false ),

    "lambdafunction_code_s3bucket"
      -> ("s3bucket", "bucket", false, true ),
    "lambdafunction_deadletterconfig_targetarn"
      -> ("null", "null", false, true ),
    "lambdafunction_function_kmskeyarn"
      -> ("kmskey", "key", false, true ),
    "lambdafunction_function_role"
      -> ("iamrole", "role", true, false ),
    "lambdafunction_vpcconfig_securitygroupids"
      -> ("ec2securitygroup", "securitygroup", true, false ),
    "lambdafunction_vpcconfig_subnetids"
      -> ("ec2subnet", "subnet", true, false ),

    "lambdaeventsourcemapping_eventsourcemapping_eventsourcearn"
      -> ("null", "null", true, true ),
    "lambdaeventsourcemapping_eventsourcemapping_functionname"
      -> ("lambdafunction", "function", true, true ),

    "lambdaalias_alias_functionname"
      -> ("lambdafunction", "function", true, true ),

    "lambdapermission_permission_functionname"
      -> ("lambdafunction", "function", true, true ),
    "lambdapermission_permission_sourcearn"
      -> ("null", "null", false, true ),

    "lambdaversion_version_functionname"
      -> ("lambdafunction", "function", true, true ),

    "snstopic_subscription_endpoint"
      -> ("null", "null", false, true ),
    "snstopic_topic_kmsmasterkeyid"
      -> ("kmskey", "key", false, true ),

    "snssubscription_subscription_endpoint"
      -> ("null", "null", false, true ),
    "snssubscription_subscription_topicarn"
      -> ("snstopic", "topic", true, true ),
    "snstopicpolicy_topicpolicy_topics"
      -> ("snstopic", "topic", true, false ),

    "sqsqueue_queue_kmsmasterkeyid"
      -> ("kmskey", "key", false, true ),

    "sqsqueuepolicy_queuepolicy_queues"
      -> ("sqsqueue", "queue", true, false ),

    "s3bucket_serversideencryptionbydefault_kmsmasterkeyid"
      -> ("kmskey", "key", false, true ),
    "s3bucket_loggingconfiguration_destinationbucketname"
      -> ("s3bucket", "bucket", false, true ),
    "s3bucket_destination_bucketarn"
      -> ("s3bucket", "bucket", true, true ),
    "s3bucket_encryptionconfiguration_replicakmskeyid"
      -> ("kmskey", "key", true, true ),
    "s3bucket_replicationconfiguration_role"
      -> ("iamrole", "role", true, true ),
    "s3bucket_replicationdestination_bucket"
      -> ("s3bucket", "bucket", true, true ),
    "s3bucket_lambdaconfiguration_function"
      -> ("lambdafunction", "function", true, true ),
    "s3bucket_queueconfiguration_queue"
      -> ("sqsqueue", "queue", true, true ),
    "s3bucket_topicconfiguration_topic"
      -> ("snstopic", "topic", true, true ),
    "s3bucketpolicy_bucketpolicy_bucket"
      -> ("s3bucket", "bucket", true, true ),

    "cloudformationcustomresource_customresource_servicetoken"
      -> (null,null,true,true),

    "configdeliverychannel_deliverychannel_s3bucketname"
      -> ("s3bucket", "bucket",true,true),
    "configdeliverychannel_deliverychannel_snstopicarn"
      -> ("snstopic","topic",false,true),

    "configconfigurationrecorder_configurationrecorder_rolearn"
      -> ("iamrole","role",true,true),

    "cloudwatchalarm_alarm_alarmactions"
      -> ("snstopic", "topic", false, false),
    "cloudwatchalarm_dimension_value"
      -> (null,null,true,true),

    "eventsrule_target_arn"
      -> ("lambdafunction", "function", true, false),

    "logssubscriptionfilter_subscriptionfilter_destinationarn"
      -> (null,null,true,true),
    "logssubscriptionfilter_subscriptionfilter_loggroupname"
      -> ("logsloggroup","loggroup",true,true),

    "logsdestination_destination_targetarn"
      -> (null,null,true,true),
    "logsdestination_destination_rolearn"
      -> ("iamrole","role",true,true),

    "route53recordset_recordset_hostedzoneid"
      -> ("route53hostedzone","hostedzone",false,true),

    "logsmetricfilter_metricfilter_loggroupname"
      -> ("logsloggroup","loggroup",true,true),

    "apigatewayaccount_account_cloudwatchrolearn"
      -> ("iamrole","role",false,true),

    "apigatewaydeployment_deployment_restapiid"
      -> ("apigatewayrestapi","restapi",true,true),

    "apigatewaybasepathmapping_basepathmapping_restapiid"
      -> ("apigatewayrestapi","restapi",true,true),

    "apigatewayusageplan_apistage_apiid"
      -> ("apigatewayrestapi","restapi",false,true),

    "apigatewaydomainname_domainname_regionalcertificatearn"
      -> (null,null,false,true),

    "kmsalias_alias_targetkeyid"
      -> ("kmskey","key",true,true),

    "apigatewayrestapi_s3location_bucket"
      -> ("s3bucket","bucket",false,true),

    // Null because it can point to any other DB Resource (RDS,Aurora,etc...)
    "rdsdbinstance_dbinstance_dbsnapshotidentifier"
      -> (null,null,false,true)

  )

}
