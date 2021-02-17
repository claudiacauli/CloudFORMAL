package com.cloud.formal.dataflow

import java.io.File

import com.cloud.formal.model.{Model, ModelIRI, ModelWriter}
import com.cloud.formal.{FilePath, Extension => Ex}
import com.typesafe.scalalogging.StrictLogging
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{AddImport, OWLDataFactory, OWLOntology, OWLOntologyAlreadyExistsException, OWLOntologyManager}

class DataflowModel
(val name: String, val ontologies: Vector[OWLOntology])
  extends Model with StrictLogging
{

  val manager :OWLOntologyManager
  = OWLManager.createOWLOntologyManager()
  val ontology: OWLOntology
  = manager.createOntology(ModelIRI.stackSetIRI(name))
  val df: OWLDataFactory
  = manager.getOWLDataFactory

  /*
  ontologies
    .foreach(o =>
      importOntology(this,o))
  */


  def importNeededOntologies = {

    new File(FilePath.ResourceTerms)
      .listFiles().filter(_.getName.endsWith(Ex.OWL))
      .foreach(f => {
        try {
          manager.applyChange(
            new AddImport(ontology,
              df.getOWLImportsDeclaration(
                manager.getOntologyDocumentIRI(manager.loadOntologyFromOntologyDocument(f)))))
        }
        catch {
          case _:OWLOntologyAlreadyExistsException =>
        }
      })

    val dfdPath = FilePath.ProjectResources + "Dataflow/"
    (List("aws.owl") ++ Ontologies.LISTDFDS)
      .foreach( s => {
      val imported = manager.loadOntologyFromOntologyDocument(new File(dfdPath + s))
      manager.applyChange(new AddImport(ontology, df.getOWLImportsDeclaration( manager.getOntologyDocumentIRI(imported) )))
    })
  }




  def writeToOutputFolder(destinationFolder: String): Unit = {
    logger.info(s"Writing $name StackSetModel and its " +
      s"imports to corresponding subfolder.")
    importNeededOntologies
    ModelWriter
      .writeDataflowToOutputFolder(this,
        destinationFolder)
  }




}
