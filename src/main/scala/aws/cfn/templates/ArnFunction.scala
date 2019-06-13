package aws.cfn.templates


protected final case class ArnFunction(optRE:Option[Json2ResourceEncoder],
                             tE: Json2TemplateEncoder)
  extends CloudFormationFunction
    with (String => Resource)
{
  def apply(arnString: String): Resource =
    SubFunction(optRE,tE)(StringNode(arnString)) match {
      case StringNode(arn) =>
        new Arn(tE.ssE.iE, arn).resourcesFromArn() match {
          case v if v.size==1 =>
            updateResourceByPolicyMap(optRE,v.head)
            v.head
          case v => ListOfResources(v)
        }
    }
}


private class Arn(iE: Json2InfrastructureEncoder, evaluatedString : String) {

  val arnComponents:(Option[String], Option[String], Option[String], Option[String], Vector[String]) = decomposeArn
  val partition   :Option[String] = arnComponents._1
  val service     :Option[String] = arnComponents._2
  val region      :Option[String] = arnComponents._3
  val account     :Option[String] = arnComponents._4
  val identifiers :Vector[String] = arnComponents._5

  var matchingResources : Vector[Resource] = Vector()


  def resourcesFromArn() : Vector[Resource] = {
    iE.resourcesByArn.getOrElse(
      evaluatedString,
      findMatchingResources
    )
  }

  private def findMatchingResources : Vector[Resource] = {

    //    println("\nHandling ARN : " + evaluatedString)
    //    println("Identifiers is the vector: " + identifiers)
    //    if ( identifiers == Vector("*") ) println ("Identifiers is all resources of that type!")
    //    if ( !(identifiers==Vector("*"))) println ("Identifiers can be a longer vector or just don't even contain *")
    //    if ( identifiers contains "*") println ( "Identifiers contain *, therefore might also contain resource name" )

    val identifiersIsStarVector = identifiers == Vector("*")

    matchingResources =
      if (identifiersIsStarVector)
        resourcesMatchingServiceType
      else {
        val rS = resourcesMatchingIdentifiers
        if (rS.nonEmpty) rS else resourcesMatchingServiceType
      }

    //    println("Matching resources: "+ matchingResources)


    if (matchingResources.nonEmpty) {
      iE.resourcesByArn = iE.resourcesByArn ++ Map(evaluatedString -> matchingResources)
      matchingResources
    }
    else {
      val newForeignNode = ExternalResource(evaluatedString,iE.infrastructure)
      iE.externalResources = iE.externalResources ++ Set(newForeignNode)
      iE.resourcesByArn = iE.resourcesByArn ++ Map(evaluatedString -> Vector(newForeignNode))
      matchingResources = Vector(newForeignNode)
      matchingResources
    }

  } ensuring matchingResources.nonEmpty


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




  private def identifiersContainEitherResourceIDorName : StackSetResource => Boolean =
    res => {
      (identifiers contains res.resourceLogicalId.toLowerCase) ||
        (identifiers contains res.resourceName.toLowerCase)
    }

  private def sameService : StackSetResource => Boolean =
    res => service match {
      case None => true
      case Some(r) => res.serviceType.toLowerCase == r
    }

  private def resourcesMatchingIdentifiers : Vector[StackSetResource] =
    resourcesMatchingCondition(identifiersContainEitherResourceIDorName)

  private def resourcesMatchingServiceType : Vector[StackSetResource] =
    resourcesMatchingCondition(sameService)


  private def resourcesMatchingCondition(condition: StackSetResource => Boolean) : Vector[StackSetResource] = {
    for ( ssE <- iE.stackSetEncoders; tE <- ssE.templatesEncoders; r <- tE.resources.toVector
          if comparePartition(tE.parameters(PseudoParameter.Partition))
            && compareAccount(tE.parameters(PseudoParameter.AccountId))
            && compareRegion(tE.parameters(PseudoParameter.Region))
            && condition(r._2)
    ) yield {
      r._2
    }
  }


}
