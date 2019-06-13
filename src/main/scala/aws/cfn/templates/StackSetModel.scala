package aws.cfn.templates

import aws.cfn.dlmodel.{Model, ModelIRI, ModelWriter}
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model._

class StackSetModel (val name: String, val ontologies: Vector[OWLOntology]) extends Model {

  val manager :OWLOntologyManager = OWLManager.createOWLOntologyManager()
  val ontology: OWLOntology = manager.createOntology(ModelIRI.stackSetIRI(name))
  val df: OWLDataFactory = manager.getOWLDataFactory

  ontologies.foreach(o => importOntology(this,o))


  def writeToOutputFolder (destinationFolder: String): Unit = {
    ModelWriter.writeStackSetToFolder(this,destinationFolder)
  }


























}