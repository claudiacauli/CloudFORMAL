package aws.cfn.dlmodel

import org.semanticweb.owlapi.model.{OWLDataFactory, OWLOntology, OWLOntologyManager}

trait DescriptionLogicModel {
  val name : String
  val manager : OWLOntologyManager
  val df : OWLDataFactory
  val ontology : OWLOntology
}
