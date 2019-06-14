package aws.cfn.mapping.templates.mapping

import aws.cfn.model.{Model, ModelIRI, ModelWriter}
import com.typesafe.scalalogging.StrictLogging
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model._

class StackSetModel
(val name: String, val ontologies: Vector[OWLOntology])
  extends Model with StrictLogging
{

  val manager :OWLOntologyManager
  = OWLManager.createOWLOntologyManager()
  val ontology: OWLOntology
  = manager.createOntology(ModelIRI.stackSetIRI(name))
  val df: OWLDataFactory
  = manager.getOWLDataFactory

  ontologies
    .foreach(o =>
      importOntology(this,o))



  def writeToOutputFolder(destinationFolder: String): Unit = {
    logger.info(s"Writing $name StackSetModel and its " +
      s"imports to corresponding subfolder.")
    ModelWriter
      .writeStackSetToFolder(this,
        destinationFolder)
  }




}