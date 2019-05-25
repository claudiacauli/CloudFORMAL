package aws.cfn.formalization

class Arn(evaluatedString : String) {


  val arnComponents: (Option[String], Option[String], Option[String], Option[String], Vector[String]) = decomposeArn
  val partition: Option[String] = arnComponents._1
  val service: Option[String] = arnComponents._2
  val region: Option[String] = arnComponents._3
  val account: Option[String] = arnComponents._4
  val identifiers: Vector[String] = arnComponents._5


  def resourceFromArn() : Node = {


    null
  }







  private def decomposeArn : (Option[String], Option[String], Option[String], Option[String], Vector[String]) = {

    val arnComponents = evaluatedString.replace("arn:","").split(":",-1)
    val part = if (arnComponents(0).equals("")) None else Some(arnComponents(0))
    val serv = if (arnComponents(1).equals("")) None else Some(arnComponents(1))
    val reg = if (arnComponents(2).equals("")) None else Some(arnComponents(2))
    val acc = if (arnComponents(3).equals("")) None else Some(arnComponents(3))
    val lastBit = evaluatedString.split(arnComponents(0)+":"+arnComponents(1)+":"+arnComponents(2)+":"+arnComponents(3)+":").last
    val ids = lastBit match {
      case s => s.replaceAll("/",":").split(":").toVector
      case "" => Vector()
    }

    (part,serv,reg,acc,ids)
  }



}