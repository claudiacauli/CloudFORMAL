package aws.cfn.templates

protected trait CloudFormationFunction {

  def updateResourceByPolicyMap(optRE: Option[Json2ResourceEncoder],
                                policy: Node): Unit =
    (policy, optRE) match {
      case (ssR:StackSetResource,Some(rE)) if rE.pointedResourceIsPolicy(ssR) =>
        rE.tE.ssE.iE.updateResByPolicyMap(ssR,rE.resource)
      case _ => ()
    }

  def updateResourceByArnMap( optRE: Option[Json2ResourceEncoder],
                              resource: Resource,
                              name: String): Unit =
    (resource,optRE) match {
      case (eR:ExternalResource,Some(rE)) if !rE.tE.ssE.resourceByArn.values.toVector.contains(eR)
      => rE.tE.ssE.foreignResourcesByArn = rE.tE.ssE.foreignResourcesByArn ++ Map( name -> eR )
    }

}
