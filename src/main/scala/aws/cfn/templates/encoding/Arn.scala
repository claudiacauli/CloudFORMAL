package aws.cfn.templates.encoding

import aws.cfn.templates.formalization._

class Arn(iE: Json2InfrastructureEncoder, evaluatedString : String) {

  val arnComponents:(Option[String], Option[String], Option[String], Option[String], Vector[String]) = decomposeArn
  val partition   :Option[String] = arnComponents._1
  val service     :Option[String] = arnComponents._2
  val region      :Option[String] = arnComponents._3
  val account     :Option[String] = arnComponents._4
  val identifiers :Vector[String] = arnComponents._5


  def resourceFromArn() :Node = {

    val matchingResources =
    for ( ssE <- iE.stackSetEncoders; tE <- ssE.templatesEncoders; r <- tE.resources.toVector
          if   comparePartition(tE.parameters("aws::partition"))
            && compareAccount(tE.parameters("aws::accountid"))
            && compareRegion(tE.parameters("aws::region"))
            && (identifiers contains r._1.toLowerCase)) yield {
      r
    }

    if (matchingResources.nonEmpty){
      iE.resourceByArn = iE.resourceByArn ++ Map(evaluatedString -> matchingResources.head._2)
      matchingResources.head._2
    }
    else {
      println("Could not find a node in the current infrastructure matching arn " + evaluatedString + " returning new foreign node.")
      val newForeignNode = ForeignResource(evaluatedString)
      iE.resourceByArn = iE.resourceByArn ++ Map(evaluatedString -> newForeignNode)
      newForeignNode
    }

    // TODO!

    // Look-up in the resourceByArn map. If it's there, return the resource
    // Look-up the information that you have from the arn in the maps that belong to the stackSetEncoder
    // If you find a matching resource, add it to the ResourceByArn map, and then return it.
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



}
