package aws.cfn.templates.formalization

class Template(val name: String) {

  var resources : Map[String,StackSetResource] = Map()

}
