package aws.cfn.templates

import java.io.File

import aws.cfn.dlmodel.{Model, ModelIRI, ModelWriter}
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{OWLDataFactory, OWLOntology, OWLOntologyManager}


object PermissionsModel{

  def fromInfrastructure(infrastructure: Infrastructure)
  : Model = {
    PermissionsModelMapper.encode(infrastructure)
  }

}


private class PermissionsModel(val name: String) extends Model {

  val manager :OWLOntologyManager = OWLManager.createOWLOntologyManager()
  val ontology: OWLOntology = manager.createOntology(ModelIRI.permissionsModelIRI(name))
  val df: OWLDataFactory = manager.getOWLDataFactory

  def writeToOutputFolder(destinationFolder : String): Unit = {
    //val dir = ModelWriter.makeDirIfDoesNotExist(destinationFolder+"/"+name)
    val dir = new File(destinationFolder+"/"+name)
    stackSetFilesInSubdirs(dir) foreach ( f => importFile(this,f) )
    ModelWriter.writePermissionToOutputFolder(this,destinationFolder)
  }




}
