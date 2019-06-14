package aws.cfn.model

import java.io.File

import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.model.parameters.ChangeApplied


trait Model {


  private[model] val name      : String
  private[model] val manager   : OWLOntologyManager
  private[model] val df        : OWLDataFactory
  private[model] val ontology  : OWLOntology


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


  protected def stackSetFilesInSubdirs(currentDir: File): Vector[File] = {
    currentDir.listFiles()
      .filter(_.isDirectory).toVector
      .flatMap(_.listFiles().toVector
        .filter (_.getName.endsWith(ModelFileSuffix.StackSet)))
  }


}
