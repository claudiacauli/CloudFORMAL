package aws.cfn.formalization

class Template(val name: String, val inputParameters: Map[String, Any]) {

  var parameters : Map[String, Either[CloudFormationNode,AnyVal]] = Map()
  var mappings : Map[String,Map[String,Either[CloudFormationNode,AnyVal]]] = Map()
  var conditions : Map[String, Either[BooleanFunction, Boolean]] = Map()
  var resources : Map[String,ResourceNode] = Map()

  def addParameters(params: Map[String, Either[CloudFormationNode, AnyVal]]) : Unit =
    parameters = params

  def addMappings(maps: Map[String, Map[String, Either[CloudFormationNode, AnyVal]]]) : Unit =
    mappings = maps

  def addConditions (conds: Map[String, Either[BooleanFunction, Boolean]]) : Unit =
    conditions = conds

  def addResources (res: Map[String, ResourceNode]) : Unit =
    resources = res


}
