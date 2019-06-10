package aws.cfn.dlmodel.template

import aws.cfn.dlmodel.{DLModel, DLModelIRI, DLModelWriter}
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{OWLDataFactory, OWLOntology, OWLOntologyManager}

class DataFlowModel(val name: String) extends DLModel {

  val manager :OWLOntologyManager = OWLManager.createOWLOntologyManager()
  val ontology: OWLOntology = manager.createOntology(DLModelIRI.stackFlowIRI(name))
  val df: OWLDataFactory = manager.getOWLDataFactory

  def writeToOutputFolder (destinationFolder: String): Unit = {
    DLModelWriter.writeDataFlowToOutputFolder(this,destinationFolder)
  }

}
