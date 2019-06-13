package aws.cfn.dlmodel.template

import aws.cfn.dlmodel.{DLModel, DLModelIRI, DLModelWriter}
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{OWLDataFactory, OWLOntology, OWLOntologyManager}

class PermissionsModel(val name: String) extends DLModel {

  val manager :OWLOntologyManager = OWLManager.createOWLOntologyManager()
  val ontology: OWLOntology = manager.createOntology(DLModelIRI.permissionsModelIRI(name))
  val df: OWLDataFactory = manager.getOWLDataFactory

  def writeToOutputFolder(destinationFolder : String): Unit = {
    val dir = DLModelWriter.makeDirIfDoesNotExist(destinationFolder+"/"+name)
    stackSetFilesInSubdirs(dir) foreach ( f => importFile(this,f) )
    DLModelWriter.writePermissionToOutputFolder(this,destinationFolder)

  }

}
