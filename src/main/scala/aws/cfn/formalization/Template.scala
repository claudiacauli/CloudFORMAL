package aws.cfn.formalization

class Template(val name: String, val inputParameters: Map[String, Any]) {


  var resources : Map[String,ResourceNode] = Map()


  def addResources (res: Map[String, ResourceNode]) : Unit =
    resources = res


}
