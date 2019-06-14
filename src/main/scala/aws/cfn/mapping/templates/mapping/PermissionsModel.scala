package aws.cfn.mapping.templates.mapping

import java.io.File

import aws.cfn.model.{Model, ModelIRI, ModelWriter}
import aws.cfn.mapping.templates.Infrastructure
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{OWLDataFactory, OWLOntology, OWLOntologyManager}


object PermissionsModel
{
  def fromInfrastructure(infrastructure: Infrastructure): Model =
    PermissionsModelMapper
      .encode(infrastructure)

}


private class PermissionsModel(val name: String)
  extends Model
{

  protected[mapping] val manager :OWLOntologyManager
  = OWLManager.createOWLOntologyManager()
  protected[mapping] val ontology: OWLOntology
  = manager.createOntology(ModelIRI.permissionsModelIRI(name))
  protected[mapping] val df: OWLDataFactory
  = manager.getOWLDataFactory


  def writeToOutputFolder(destinationFolder: String): Unit =
  {
    val dir = new File(destinationFolder+"/"+name)
    stackSetFilesInSubdirs(dir)
      .foreach(f =>
        importFile(this,f))
    ModelWriter
      .writePermissionToOutputFolder(
        this,destinationFolder)
  }




}
