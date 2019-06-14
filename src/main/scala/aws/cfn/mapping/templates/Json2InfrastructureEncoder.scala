package aws.cfn.mapping.templates

import argonaut.Json
import com.typesafe.scalalogging.{LazyLogging, StrictLogging}

import scala.collection.mutable

object Json2InfrastructureEncoder {

  def encode(stackSets: Vector[(Vector[(String,Json,Option[Json])],String)], infrastructureName:String): Infrastructure = {
    new Json2InfrastructureEncoder(stackSets,infrastructureName).encode()
  }

}


private class Json2InfrastructureEncoder
(stackSets: Vector[(Vector[(String,Json,Option[Json])],String)], infrastructureName:String)
extends StrictLogging
{

  logger.info(s"Initializing $infrastructureName Infrastructure Encoder and sub-Encoders.")

  AwsManagedPolicies.loadManagedPolicies()

  private[templates] val infrastructure =
    new Infrastructure(infrastructureName)

  private[templates] val stackSetEncoders =
    stackSets map (ss => new Json2StackSetEncoder(this,ss._1,ss._2))

  private[templates] var resourcesByArn
  : Map[String,Vector[Resource]] = Map()

  private[templates] var resourcesPointingToPolicy
  : Map[StackSetResource,mutable.Set[StackSetResource]] = Map()

  private[templates] var externalResources
  : Set[ExternalResource] = Set()

  private[templates] var policyStatements
  : Set[Statement] = Set()

  logger.info(s"Initialization of $infrastructureName Infrastructure Encoder and sub-Encoders completed.")

  private def encode(): Infrastructure = {
    stackSetEncoders foreach (_.updateResourcesNames())
    logger.info("Binding Resources names, in addition to their logical IDs.")
    stackSetEncoders foreach (_.encode())
    logger.info("Fully instantiated all Resource objects and their properties.")
    infrastructure.stackSets = (stackSetEncoders map (_.encodePolicies())).toSet
    logger.info("Computed Policy Statements for the entire infrastructure.")
    infrastructure.aclStatements = policyStatements
    infrastructure.externalResources = this.externalResources
    logger.info(s"Generation of Infrastructure Object for $infrastructureName completed.")
    infrastructure
  }



  private[templates]
  def updateResByPolicyMap
  (policyRes: StackSetResource, resource: StackSetResource): Unit =
    resourcesPointingToPolicy.get(policyRes) match {
      case None     =>
        resourcesPointingToPolicy ++= Map(policyRes -> mutable.Set(resource))
      case Some(_)  =>
        resourcesPointingToPolicy(policyRes).add(resource)
    }





}