package aws.cfn.templates

import java.io.File

import org.semanticweb.owlapi.model.OWLOntologyManager

protected class StackSet( val name:String,
                val infrastructure: Infrastructure,
                val manager: OWLOntologyManager) {

  var templates: Vector[Template] = Vector()
  var foreignNodes: Map[String,ExternalResource] = Map()
  manager.loadOntologyFromOntologyDocument(new File("src/main/resources/terminology/aws.owl"))

  override def toString: String = {
    "\tStackSet: " + name + ", includes Templates: " + "\n" +
      templates.foldLeft("")((a,b)=> a + b.toString + "\n")
  }

}
