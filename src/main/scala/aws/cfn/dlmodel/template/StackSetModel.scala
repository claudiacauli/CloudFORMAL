package aws.cfn.dlmodel.template

import aws.cfn.dlmodel.{DLModel, DLModelWriter, DLModelIRI}
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{AddImport, OWLDataFactory, OWLOntology, OWLOntologyManager}

class StackSetModel (val name: String, val ontologies: Vector[OWLOntology]) extends DLModel {

  val manager :OWLOntologyManager = OWLManager.createOWLOntologyManager()
  val ontology: OWLOntology = manager.createOntology(DLModelIRI.resourceTerminologyIRI(name))
  val df: OWLDataFactory = manager.getOWLDataFactory

  ontologies.foreach(o => {
    manager.loadOntology( o.getOWLOntologyManager.getOntologyDocumentIRI(o))
    manager.applyChange( new AddImport(ontology, df.getOWLImportsDeclaration(o.getOWLOntologyManager.getOntologyDocumentIRI(o))))
  })


  def writeToOutputFolder (destinationFolder: String) = {
    DLModelWriter.writeStackSetToOutputFolder(this,destinationFolder)
  }

}