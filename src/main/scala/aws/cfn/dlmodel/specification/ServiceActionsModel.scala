package aws.cfn.dlmodel.specification

import aws.cfn.dlmodel.{DLModel, DLModelWriter, DLModelIRI}
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{OWLDataFactory, OWLOntology, OWLOntologyManager}

class ServiceActionsModel(val name:String) extends DLModel{

  val manager: OWLOntologyManager = OWLManager.createOWLOntologyManager()
  val ontology: OWLOntology = manager.createOntology(DLModelIRI.actionsOntologyIRI(name))
  val df: OWLDataFactory = manager.getOWLDataFactory

  def writeToOutputFolder (destinationFolder: String): Unit = {
    DLModelWriter.writeSpecificationToOutputFolder(this,destinationFolder)
  }

}
