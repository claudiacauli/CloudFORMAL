package aws.cfn.templates.formalization

import java.io.File

import org.semanticweb.owlapi.model.OWLOntologyManager

class StackSet(val name:String, val manager: OWLOntologyManager) {

  var templates: Vector[Template] = Vector()
  var foreignNodes: Map[String,ForeignResource] = Map()
  manager.loadOntologyFromOntologyDocument(new File("src/main/resources/terminology/policy/policydocument.owl"))

}
