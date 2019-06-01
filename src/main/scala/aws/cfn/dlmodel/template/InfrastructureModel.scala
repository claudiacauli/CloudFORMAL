package aws.cfn.dlmodel.template

import java.io.File

import aws.cfn.dlmodel.{DLModel, DLModelIRI, DLModelWriter}
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{OWLDataFactory, OWLOntology, OWLOntologyManager}



class InfrastructureModel(val name:String, stacksetsModels: Vector[StackSetModel]) extends DLModel {

  val manager :OWLOntologyManager = OWLManager.createOWLOntologyManager()
  val ontology: OWLOntology = manager.createOntology(DLModelIRI.infrastructureModelIRI(name))
  val df: OWLDataFactory = manager.getOWLDataFactory



  def writeToOutputFolder (destinationFolder: String): Unit = {

    val dir = DLModelWriter.makeDirIfDoesNotExist(destinationFolder+"/"+name)
    stacksetsModels foreach  (ssM => DLModelWriter.writeStackSetToOutputFolder(ssM,destinationFolder+"/"+name+"/",""))
    stackSetFilesInSubdirs(dir) foreach ( f => importFile(this,f) )
    DLModelWriter.writeInfrastructureToOutputFolder(this,destinationFolder)

  }



  private def stackSetFilesInSubdirs(currentDir: File) = {
    (currentDir.listFiles() filter (f => f.isDirectory)).toVector flatMap (
      f => f.listFiles().toVector filter (f => f.getName.endsWith(DLModelWriter.stackSetFileSuffix))
    )
  }



}
