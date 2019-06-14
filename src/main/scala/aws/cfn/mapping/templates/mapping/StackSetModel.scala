package aws.cfn.mapping.templates.mapping

import aws.cfn.model.{Model, ModelIRI, ModelWriter}
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model._

class StackSetModel
(val name: String, val ontologies: Vector[OWLOntology])
  extends Model
{

  protected[mapping] val manager :OWLOntologyManager
  = OWLManager.createOWLOntologyManager()
  protected[mapping] val ontology: OWLOntology
  = manager.createOntology(ModelIRI.stackSetIRI(name))
  protected[mapping] val df: OWLDataFactory
  = manager.getOWLDataFactory

  ontologies
    .foreach(o =>
      importOntology(this,o))



  def writeToOutputFolder(destinationFolder: String): Unit =
    ModelWriter
      .writeStackSetToFolder(this,
        destinationFolder)



}