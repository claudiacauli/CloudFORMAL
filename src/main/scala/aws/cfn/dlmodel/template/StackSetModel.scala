package aws.cfn.dlmodel.template

import aws.cfn.dlmodel.{DLModel, DLModelIRI, DLModelWriter}
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model._

class StackSetModel (val name: String, val ontologies: Vector[OWLOntology]) extends DLModel {

  val manager :OWLOntologyManager = OWLManager.createOWLOntologyManager()
  val ontology: OWLOntology = manager.createOntology(DLModelIRI.stackSetIRI(name))
  val df: OWLDataFactory = manager.getOWLDataFactory

  ontologies.foreach(o => importOntology(this,o))




  def writeToOutputFolder (destinationFolder: String): Unit = {
    DLModelWriter.writeStackSetToOutputFolder(this,destinationFolder,"")
  }


























}