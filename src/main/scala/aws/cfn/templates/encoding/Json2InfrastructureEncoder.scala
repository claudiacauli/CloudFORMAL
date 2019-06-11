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

  val stackSetEncoders: Vector[Json2StackSetEncoder] = stackSets map (ss => new Json2StackSetEncoder(this,ss._1,ss._2))
  var resourcesByArn: Map[String,Vector[Entity]] = Map()
  var resourcesPointingToPolicy: Map[Resource,mutable.Set[Resource]] = Map()

  var allPoliciesStatement : Set[Statement] = Set()

  def encode(): Infrastructure = {
    stackSetEncoders foreach (ssE => ssE.updateResourcesNames())
    stackSetEncoders foreach (ssE => ssE.encode())
    val infr = new Infrastructure(infrastructureName,
          stackSetEncoders map (ssE => ssE.encodePolicies()) )
    println(this)

//    stackSetEncoders foreach ( ssE => ssE.templatesEncoders foreach (
//      tE => tE.resources foreach ( r => println(r._2.resourceLogicalId + " ---> "  +r._2.resourceName) )
//    ) )

    infr
  }


  override def toString: String = {
    "\nInfrastructure: " + infrastructureName + ", includes StackSets: \n" +
      stackSetEncoders.foldLeft("")((a,b)=> a + " - " + b.stackSet.name + " \n")  + "\n" +
      stackSetEncoders.foldLeft("")((a,b)=> a + b.toString + "\n")
  }


  def updateResByPolicyMap(policyRes: Resource, resource: Resource): Unit = {
    if (resourcesPointingToPolicy.get(policyRes).isDefined) {
      resourcesPointingToPolicy(policyRes).add(resource)
    } else
      resourcesPointingToPolicy = resourcesPointingToPolicy ++ Map(policyRes -> mutable.Set(resource))
  }

}