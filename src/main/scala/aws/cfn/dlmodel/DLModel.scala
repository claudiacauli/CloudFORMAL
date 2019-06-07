package aws.cfn.dlmodel

import java.io.File

import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.model.parameters.ChangeApplied

trait DLModel {

  val name : String
  val manager : OWLOntologyManager
  val df : OWLDataFactory
  val ontology : OWLOntology


  def writeToOutputFolder (destinationFolder: String)

  def importOntology(model: DLModel, o: OWLOntology): ChangeApplied =
    importDocIRI(model, o.getOWLOntologyManager.getOntologyDocumentIRI(o))


  def importDocIRI(model:DLModel, docIRI: IRI): ChangeApplied = {
    model.manager.loadOntology( docIRI )
    model.manager.applyChange( new AddImport( model.ontology, model.df.getOWLImportsDeclaration( docIRI ) ))
  }

  def importFile(model:DLModel, owlFile: File): ChangeApplied = {
    val o = manager.loadOntologyFromOntologyDocument(owlFile)
    model.manager.applyChange( new AddImport( model.ontology, model.df.getOWLImportsDeclaration( o.getOWLOntologyManager.getOntologyDocumentIRI(o))))
  }


}
