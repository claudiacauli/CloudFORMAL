package aws.cfn.templates.encoding

import aws.cfn.templates.formalization._

class Arn(iE: Json2InfrastructureEncoder, evaluatedString : String) {

  val arnComponents:(Option[String], Option[String], Option[String], Option[String], Vector[String]) = decomposeArn
  val partition   :Option[String] = arnComponents._1
  val service     :Option[String] = arnComponents._2
  val region      :Option[String] = arnComponents._3
  val account     :Option[String] = arnComponents._4
  val identifiers :Vector[String] = arnComponents._5


  def resourcesFromArn() : Vector[Entity] = {
    iE.resourcesByArn.getOrElse(
      evaluatedString,
      findMatchinResources
    )
  }

  private def findMatchinResources : Vector[Entity] = {

//    println("\nHandling ARN : " + evaluatedString)
//    println("Identifiers is the vector: " + identifiers)
//    if ( identifiers == Vector("*") ) println ("Identifiers is all resources of that type!")
//    if ( !(identifiers==Vector("*"))) println ("Identifiers can be a longer vector or just don't even contain *")
//    if ( identifiers contains "*") println ( "Identifiers contain *, therefore might also contain resource name" )

    val identifiersIsStarVector = identifiers == Vector("*")

    val matchingResources =
      if (identifiersIsStarVector)
        resourcesMatchinServiceType
      else {
        val rS = resourcesMatchingIdentifiers
        if (rS.nonEmpty) rS else resourcesMatchinServiceType
      }

//    println("Matching resources: "+ matchingResources)

    if (matchingResources.nonEmpty) {
      iE.resourcesByArn = iE.resourcesByArn ++ Map(evaluatedString -> matchingResources)
      matchingResources
    }
    else {
      val newForeignNode = ExternalEntity(evaluatedString,iE.infrastructure)
      iE.resourcesByArn = iE.resourcesByArn ++ Map(evaluatedString -> Vector(newForeignNode))
      Vector(newForeignNode)
    }

  }


  private def compareAccount(accountTemplateNode : GenericValueNode): Boolean = {
    account match {
      case None => true
      case Some(a) => accountTemplateNode.asInstanceOf[StringNode].value == a
    }
  }

  private def comparePartition(accountPartitionNode : GenericValueNode) :Boolean = {
    partition match {
      case None => true
      case Some(p) => accountPartitionNode.asInstanceOf[StringNode].value == p
    }
  }

  private def compareRegion(accountRegionNode : GenericValueNode) : Boolean= {
    region match {
      case None => true
      case Some(r) => accountRegionNode.asInstanceOf[StringNode].value == r
    }
  }






  def str(comp: Option[String]) : String = comp match {
    case None => " "
    case Some(s) => s+" "
  }







  private def decomposeArn : (Option[String], Option[String], Option[String], Option[String], Vector[String]) = {

    val arnComponents = evaluatedString.replace("arn:","").split(":",-1)
    val part = if (arnComponents(0).equals("")) None else Some(arnComponents(0))
    val serv = if (arnComponents(1).equals("")) None else Some(arnComponents(1))
    val reg = if (arnComponents(2).equals("")) None else Some(arnComponents(2))
    val acc = if (arnComponents(3).equals("")) None else Some(arnComponents(3))
    val lastBit = evaluatedString.split(arnComponents(0)+":"+arnComponents(1)+":"+arnComponents(2)+":"+arnComponents(3)+":").last
    val ids = lastBit match {
      case "" => Vector()
      case  s => s.replaceAll("/",":").split(":").toVector
    }

    (part,serv,reg,acc,ids)
  }




  private def identifiersContainEitherResourceIDorName : Resource => Boolean =
    res => {
      (identifiers contains res.resourceLogicalId.toLowerCase) ||
        (identifiers contains res.resourceName.toLowerCase)
    }

  private def sameService : Resource => Boolean =
    res => service match {
        case None => true
        case Some(r) => res.serviceType.toLowerCase == r
      }

  private def resourcesMatchingIdentifiers : Vector[Resource] =
    resourcesMatchingCondition(identifiersContainEitherResourceIDorName)

  private def resourcesMatchinServiceType : Vector[Resource] =
    resourcesMatchingCondition(sameService)


  private def resourcesMatchingCondition(condition: Resource => Boolean) : Vector[Resource] = {
    for ( ssE <- iE.stackSetEncoders; tE <- ssE.templatesEncoders; r <- tE.resources.toVector
          if   comparePartition(tE.parameters("aws::partition"))
            && compareAccount(tE.parameters("aws::accountid"))
            && compareRegion(tE.parameters("aws::region"))
            && condition(r._2)
    ) yield {
      r._2
    }
  }


}
