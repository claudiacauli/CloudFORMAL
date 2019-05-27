package aws.cfn.dlmodel.specification

import aws.cfn.dlmodel.{DLModel, DLModelWriter, DLModelIRI}
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{OWLDataFactory, OWLOntology, OWLOntologyManager}

class ResourceSpecificationModel(val name : String) extends DLModel{

  val manager: OWLOntologyManager = OWLManager.createOWLOntologyManager()
  val ontology: OWLOntology = manager.createOntology(DLModelIRI.resourceTerminologyIRI(name))
  val df: OWLDataFactory = manager.getOWLDataFactory

  def writeToOutputFolder (destinationFolder: String) = {
    DLModelWriter.writeSpecificationToOutputFolder(this,destinationFolder)
  }

}
