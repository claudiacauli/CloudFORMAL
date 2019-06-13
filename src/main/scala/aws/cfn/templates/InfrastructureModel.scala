package aws.cfn.templates

import java.io.File

import aws.cfn.dlmodel.{Model, ModelIRI, ModelWriter}
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model._


object InfrastructureModel
{
  def fromInfrastructure(infrastructure: Infrastructure): Model =
    InfrastructureModelMapper.encode(infrastructure)

}


private class InfrastructureModel
(val stacksetsModels: Set[StackSetModel], val infrastructure:Infrastructure)
  extends Model
{

  val name: String = infrastructure.name
  val manager :OWLOntologyManager = OWLManager.createOWLOntologyManager()
  val ontology: OWLOntology = manager.createOntology(ModelIRI.infrastructureModelIRI(name))
  val df: OWLDataFactory = manager.getOWLDataFactory



  def writeToOutputFolder(destinationFolder: String): Unit = {

    val destFolder =
      if (!destinationFolder.endsWith("/"))
        destinationFolder+"/"
      else destinationFolder

    val dir = new File(destFolder+name)
    if (!dir.exists)  dir.mkdir

    stacksetsModels
      .foreach( ssM =>
        ModelWriter.writeStackSetToFolder(
          ssM,
          destFolder+name+"/"))

    stackSetFilesInSubdirs(dir)
      .foreach( f => importFile(this,f) )

    ModelWriter.writeInfrastructureToOutputFolder(this,destFolder)
  }




}
