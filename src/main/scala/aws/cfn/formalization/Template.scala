package aws.cfn.formalization

class Template(val name: String) {


  var resources : Map[String,ResourceNode] = Map()


  def addResources (res: Map[String, ResourceNode]) : Unit =
    resources = res


}
