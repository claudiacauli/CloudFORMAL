package aws.cfn.templates.encoding

import argonaut.Json
import aws.cfn.templates.formalization._

import scala.collection.mutable

object Json2InfrastructureEncoder {

  def encode(stackSets: Vector[(Vector[(String,Json,Option[Json])],String)], infrastructureName:String): Infrastructure = {
    new Json2InfrastructureEncoder(stackSets,infrastructureName).encode()
  }

}


class Json2InfrastructureEncoder(stackSets: Vector[(Vector[(String,Json,Option[Json])],String)], infrastructureName:String){

  val infrastructure = new Infrastructure(infrastructureName)
  val stackSetEncoders  : Vector[Json2StackSetEncoder] = stackSets map (ss => new Json2StackSetEncoder(this,ss._1,ss._2))
  var resourcesByArn    : Map[String,Vector[Entity]] = Map()
  var resourcesPointingToPolicy : Map[Resource,mutable.Set[Resource]] = Map()
  var policyStatements : Set[Statement] = Set()

  def encode(): Infrastructure = {
    stackSetEncoders foreach (ssE => ssE.updateResourcesNames())
    stackSetEncoders foreach (ssE => ssE.encode())
    infrastructure.stackSets = (stackSetEncoders map (ssE => ssE.encodePolicies()) ).toSet
    infrastructure.aclStatements = policyStatements
    infrastructure
  }


  def updateResByPolicyMap(policyRes: Resource, resource: Resource): Unit = {
    if (resourcesPointingToPolicy.get(policyRes).isDefined) {
      resourcesPointingToPolicy(policyRes).add(resource)
    } else
      resourcesPointingToPolicy = resourcesPointingToPolicy ++ Map(policyRes -> mutable.Set(resource))
  }

}