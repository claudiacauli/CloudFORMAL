package aws.cfn.templates.formalization

import java.io.File

import org.semanticweb.owlapi.model.OWLOntologyManager

class StackSet( val name:String,
                val infrastructure: Infrastructure,
                val manager: OWLOntologyManager) {

  var templates: Vector[Template] = Vector()
  var foreignNodes: Map[String,ExternalEntity] = Map()
  manager.loadOntologyFromOntologyDocument(new File("src/main/resources/terminology/policy/policydocument.owl"))

  override def toString: String = {
    "\tStackSet: " + name + ", includes Templates: " + "\n" +
      templates.foldLeft("")((a,b)=> a + b.toString + "\n")
  }

}
