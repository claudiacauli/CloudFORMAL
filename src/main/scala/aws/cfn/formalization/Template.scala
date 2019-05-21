package aws.cfn.formalization

class Template(val name: String) {

  var resources : Map[String,ResourceNode] = Map()

}
