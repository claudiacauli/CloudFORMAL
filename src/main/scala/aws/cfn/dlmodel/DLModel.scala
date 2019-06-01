package aws.cfn.dlmodel

import java.io.File

import org.semanticweb.owlapi.model._

trait DLModel {

  val name : String
  val manager : OWLOntologyManager
  val df : OWLDataFactory
  val ontology : OWLOntology


  def writeToOutputFolder (destinationFolder: String)

  def importOntology(model: DLModel, o: OWLOntology) =
    importDocIRI(model, o.getOWLOntologyManager.getOntologyDocumentIRI(o))


  def importDocIRI(model:DLModel, docIRI: IRI) = {
    model.manager.loadOntology( docIRI )
    model.manager.applyChange( new AddImport( model.ontology, model.df.getOWLImportsDeclaration( docIRI ) ))
  }

  def importFile(model:DLModel, owlFile: File) = {
    val o = manager.loadOntologyFromOntologyDocument(owlFile)
    model.manager.applyChange( new AddImport( model.ontology, model.df.getOWLImportsDeclaration( o.getOWLOntologyManager.getOntologyDocumentIRI(o))))
  }


}
