package aws.cfn.dlmodel.specification

import aws.cfn.dlmodel.{DescriptionLogicModel, Symbols}
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{OWLDataFactory, OWLOntology, OWLOntologyManager}

class ServiceActionsModel(val name:String) extends DescriptionLogicModel{

  val manager: OWLOntologyManager = OWLManager.createOWLOntologyManager()
  val ontology: OWLOntology = manager.createOntology(Symbols.actionsOntologyIRI(name))
  val df: OWLDataFactory = manager.getOWLDataFactory

}
