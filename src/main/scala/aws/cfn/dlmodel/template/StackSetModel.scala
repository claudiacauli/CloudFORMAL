package aws.cfn.dlmodel.template

import aws.cfn.dlmodel.{DescriptionLogicModel, Symbols}
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{OWLDataFactory, OWLOntology, OWLOntologyManager}

class StackSetModel (val name: String) extends DescriptionLogicModel {

  val manager: OWLOntologyManager = OWLManager.createOWLOntologyManager()
  val ontology: OWLOntology = manager.createOntology(Symbols.resourceTerminologyIRI(name))
  val df: OWLDataFactory = manager.getOWLDataFactory

}
