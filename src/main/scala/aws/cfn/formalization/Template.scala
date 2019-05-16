package aws.cfn.formalization

class Template(val name: String, val inputParameters: Map[String, Any]) {

  var parameters : Map[String, Either[CloudFormationNode,AnyVal]] = Map()
  var mappings : Map[String,Map[String,Either[CloudFormationNode,AnyVal]]] = Map()
  var conditions : Map[String, Either[BooleanFunction, Boolean]] = Map()
  var resources : Map[String,ResourceNode] = Map()

}
