package aws.cfn.dlmodel

import org.semanticweb.owlapi.model.{OWLDataFactory, OWLOntology, OWLOntologyManager}

trait DLModel {

  val name : String
  val manager : OWLOntologyManager
  val df : OWLDataFactory
  val ontology : OWLOntology


  def writeToOutputFolder (destinationFolder: String)

}
