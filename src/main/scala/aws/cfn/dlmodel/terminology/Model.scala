package aws.cfn.dlmodel.terminology

import org.semanticweb.owlapi.model.{OWLDataFactory, OWLOntology, OWLOntologyManager}

trait Model {
  val name : String
  val manager : OWLOntologyManager
  val df : OWLDataFactory
  val ontology : OWLOntology
}
