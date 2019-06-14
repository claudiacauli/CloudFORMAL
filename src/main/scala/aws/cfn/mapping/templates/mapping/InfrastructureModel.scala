package aws.cfn.mapping.templates.mapping

import java.io.File

import aws.cfn.model.{Model, ModelIRI, ModelWriter}
import aws.cfn.mapping.templates.Infrastructure
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

  protected[this] val name
  : String = infrastructure.name
  protected[this] val manager
  : OWLOntologyManager = OWLManager.createOWLOntologyManager()
  protected[this] val ontology
  : OWLOntology = manager.createOntology(ModelIRI.infrastructureModelIRI(name))
  protected[this] val df
  : OWLDataFactory = manager.getOWLDataFactory



  def writeToOutputFolder(destinationFolder: String): Unit =
  {
    val outputDir = withTrailingSlash(destinationFolder)
    val dir = new File(outputDir+name)
    if (!dir.exists) dir.mkdir

    stacksetsModels
      .foreach( ssM =>
        ModelWriter.writeStackSetToFolder(
          ssM,
          outputDir+name+"/"))

    stackSetFilesInSubdirs(dir)
      .foreach(
        f => importFile(this,f))

    ModelWriter
      .writeInfrastructureToOutputFolder(this,outputDir)
  }


  private def withTrailingSlash(str: String) =
    str + (if (!str.endsWith("/")) "/" else "")




}
