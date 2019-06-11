package aws.cfn.templates.encoding

import argonaut.Json
import aws.cfn.templates.formalization.{Infrastructure, Node, StackSetResource}

import scala.collection.mutable

object Json2InfrastructureEncoder {

  def encode(stackSets: Vector[(Vector[(String,Json,Option[Json])],String)], infrastructureName:String): Infrastructure = {
    new Json2InfrastructureEncoder(stackSets,infrastructureName).encode()
  }

}


class Json2InfrastructureEncoder(stackSets: Vector[(Vector[(String,Json,Option[Json])],String)], infrastructureName:String){

  val stackSetEncoders: Vector[Json2StackSetEncoder] = stackSets map (ss => new Json2StackSetEncoder(this,ss._1,ss._2))
  var resourceByArn: Map[String,Node] = Map()
  var resourcesByPolicy: Map[StackSetResource,mutable.Set[StackSetResource]] = Map()

  def encode(): Infrastructure = {
    stackSetEncoders foreach (ssE => ssE.updateResourcesNames())
    val infr = new Infrastructure(infrastructureName,
          stackSetEncoders map (ssE => ssE.encode()) )
    println(this)
    infr
  }


  override def toString: String = {
    "\nInfrastructure: " + infrastructureName + ", includes StackSets: \n" +
      stackSetEncoders.foldLeft("")((a,b)=> a + " - " + b.stackSet.name + " \n")  + "\n" +
      stackSetEncoders.foldLeft("")((a,b)=> a + b.toString + "\n")
  }


  def updateResByPolicyMap(policyRes: StackSetResource, resource: StackSetResource): Unit = {
    if (resourcesByPolicy.get(policyRes).isDefined) {
      resourcesByPolicy(policyRes).add(resource)
    } else
      resourcesByPolicy = resourcesByPolicy ++ Map(policyRes -> mutable.Set(resource))
  }

}