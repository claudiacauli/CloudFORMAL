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

