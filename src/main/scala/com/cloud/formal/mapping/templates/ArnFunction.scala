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

import com.typesafe.scalalogging.LazyLogging


private final case class ArnFunction(optRE:Option[Json2ResourceEncoder],
                             tE: Json2TemplateEncoder)
  extends CloudFormationFunction
    with (String => Resource)
{
  def apply(arnString: String): Resource =
    SubFunction(optRE,tE)(StringNode(arnString)) match {
      case StringNode(arn) =>
        new Arn(tE.ssE.iE, arn).resourcesFromArn() match {
          case v if v.size == 1 =>
            v.head
          case v => //ListOfResources(v)
            v.head
        }
    }
}




private class Arn(iE: Json2InfrastructureEncoder, evaluatedString : String)
extends LazyLogging
{

  private val arnComponents = splitArn
  private val partition     = arnComponents._1
  private val service       = arnComponents._2
  private val region        = arnComponents._3
  private val account       = arnComponents._4
  private val identifiers   = arnComponents._5
  private var matchingResources: Vector[Resource] = Vector()


  private[templates]
  def resourcesFromArn(): Vector[Resource] =
      iE.resourcesByArn.getOrElse(
        evaluatedString,
        findMatchingResources)



  private def findMatchingResources: Vector[Resource] = {

      val identifiersIsStarVector =
        identifiers == Vector("*")

      if (identifiers == Vector(""))
        logger.warn(s"Arn $evaluatedString does not appear to have" +
          s" any identifier.")

      matchingResources =
        if (identifiersIsStarVector)
          resourcesMatchingServiceType
        else {
          val rS = resourcesMatchingIdentifiers
          if (rS.nonEmpty)
            rS
          else resourcesMatchingServiceType
        }

      if (matchingResources.nonEmpty) {
        iE.resourcesByArn ++= Map(evaluatedString -> matchingResources)
        matchingResources
      }
      else {
        val newForeignNode = ExternalResource(evaluatedString, iE.infrastructure)
        iE.externalResources ++= Set(newForeignNode)
        iE.resourcesByArn ++= Map(evaluatedString -> Vector(newForeignNode))
        Vector(newForeignNode)
      }
  }



  private def splitArn = {

    val arnComponents = evaluatedString
      .replace("arn:","")
      .split(":",-1)

    if (arnComponents.size>=5)
    {
      val partition   = if (arnComponents(0).equals("")) None else Some(arnComponents(0))
      val service     = if (arnComponents(1).equals("")) None else Some(arnComponents(1))
      val region      = if (arnComponents(2).equals("")) None else Some(arnComponents(2))
      val account     = if (arnComponents(3).equals("")) None else Some(arnComponents(3))

      val lastPart = evaluatedString
        .split(arnComponents(0)+":"+arnComponents(1)+":"+arnComponents(2)+":"+arnComponents(3)+":")
        .last

      val ids = lastPart match {
        case "" => Vector()
        case s => s.replaceAll("/",":").replaceAll("%20"," ")
          .split(":")
          .toVector
      }

      (partition,service,region,account,ids)
    }
    else
    {
      val ids = evaluatedString match {
        case "" => Vector()
        case s => s.replaceAll("/",":").replaceAll("%20"," ")
          .split(":")
          .toVector
      }
      (None,None,None,None,ids)
    }

  }


  private def resourcesMatchingIdentifiers=
    resourcesMatchingCondition(
      identifiersContainEitherResourceIDorName)


  private def identifiersContainEitherResourceIDorName
  : StackSetResource => Boolean =
    res =>
      (identifiers contains res.resourceLogicalId.toLowerCase) ||
        (identifiers contains res.resourceName.toLowerCase)


  private def resourcesMatchingServiceType  =
    resourcesMatchingCondition(sameService)


  private def sameService
  : StackSetResource => Boolean =
    res =>
      service match {
        case None => true
        case Some(r) => res.serviceType.toLowerCase == r
      }


  private def resourcesMatchingCondition(conditionFun: StackSetResource => Boolean)
  : Vector[StackSetResource] =
    for(ssE <- iE.stackSetEncoders;
        tE  <- ssE.templatesEncoders;
        r   <- tE.resources.toVector
          if samePartitionAs(tE.parameters(PseudoParameter.Partition))
            && sameAccountAs(tE.parameters(PseudoParameter.AccountId))
            && sameRegionAs(tE.parameters(PseudoParameter.Region))
            && conditionFun(r._2)
    ) yield
      r._2


  private def sameAccountAs(templateAccountNode: GenericValueNode) =
    account match {
      case None     => true
      case Some(a)  => templateAccountNode.asInstanceOf[StringNode]
        .value == a
    }


  private def samePartitionAs(templatePartitionNode: GenericValueNode) =
    partition match {
      case None     => true
      case Some(p)  => templatePartitionNode.asInstanceOf[StringNode]
        .value == p
    }


  private def sameRegionAs(templateRegionNode: GenericValueNode) =
    region match {
      case None     => true
      case Some(r)  => templateRegionNode.asInstanceOf[StringNode]
        .value == r
    }


}
