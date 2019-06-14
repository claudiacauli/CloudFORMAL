package com.claudiacauli.www.cloudformal.mapping.templates

private trait CloudFormationFunction {


  protected[this]
  def updateResourceByPolicyMap( optRE: Option[Json2ResourceEncoder],
                                 policy: Node): Unit =
    (policy, optRE) match {
      case (ssR:StackSetResource,Some(rE)) if rE.pointedResourceIsPolicy(ssR) =>
        rE.tE.ssE.iE.updateResByPolicyMap(ssR,rE.resource)
      case _ => ()
    }


  protected[this]
  def updateResourceByArnMap( optRE: Option[Json2ResourceEncoder],
                              resource: Resource,
                              name: String): Unit =
    (resource,optRE) match {
      case (eR: ExternalResource, Some(rE))
        => rE.tE.ssE.foreignResourcesByArn ++= Map(name -> eR)
      case _ => ()
    }


}

