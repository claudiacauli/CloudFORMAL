package aws.cfn.model

import java.io.File

import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.model.parameters.ChangeApplied


trait Model {


  val name      : String
  val manager   : OWLOntologyManager
  val df        : OWLDataFactory
  val ontology  : OWLOntology


  def writeToOutputFolder (destinationFolder: String)


  def importOntology(model: Model, o: OWLOntology): Unit =
    importDocIRI(
      model,
      o.getOWLOntologyManager.getOntologyDocumentIRI(o))


  private def importDocIRI(model: Model, docIRI: IRI) = {
    try {
      model.manager.loadOntology(docIRI)
      model.manager.applyChange(
        new AddImport(
          model.ontology,model.df.getOWLImportsDeclaration(docIRI)))
    } catch {
      case e:OWLOntologyDocumentAlreadyExistsException => e.getMessage
    }
  }


  def importFile(model: Model, owlFile: File): ChangeApplied = {
    val o = model.manager.loadOntologyFromOntologyDocument(owlFile)
    model.manager.applyChange(
      new AddImport(
        model.ontology,
        model.df.getOWLImportsDeclaration(
          o.getOntologyID.getOntologyIRI.get))

    )
  }


  def stackSetFilesInSubdirs(currentDir: File): Vector[File] = {
    currentDir.listFiles()
      .filter( f => f.isDirectory ).toVector
      .flatMap( f => f.listFiles().toVector
        .filter ( f => f.getName.endsWith(ModelFileSuffix.StackSet)))
  }


}
