package aws.cfn.dlmodel

import java.io.File

import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.model.parameters.ChangeApplied

trait DLModel {

  val name      : String
  val manager   : OWLOntologyManager
  val df        : OWLDataFactory
  val ontology  : OWLOntology


  def writeToOutputFolder (destinationFolder: String)

  def importOntology(model: DLModel, o: OWLOntology): Unit =
    importDocIRI(model, o.getOWLOntologyManager.getOntologyDocumentIRI(o))


  def importDocIRI(model:DLModel, docIRI: IRI) : Unit =
  {
    try {
      model.manager.loadOntology( docIRI )
      model.manager.applyChange( new AddImport( model.ontology, model.df.getOWLImportsDeclaration( docIRI ) ))
    } catch {
      case e: OWLOntologyDocumentAlreadyExistsException =>
    }
  }

  def importFile(model:DLModel, owlFile: File): Unit =
  {
    val o = manager.loadOntologyFromOntologyDocument(owlFile)
    model.manager.applyChange(
      new AddImport(
        model.ontology,
        model.df.getOWLImportsDeclaration( o.getOWLOntologyManager.getOntologyDocumentIRI(o)))
    )
  }

  def stackSetFilesInSubdirs(currentDir: File) = {
    (currentDir.listFiles() filter (f => f.isDirectory)).toVector flatMap (
      f => f.listFiles().toVector filter (f => f.getName.endsWith(DLModelWriter.stackSetFileSuffix))
      )
  }

}