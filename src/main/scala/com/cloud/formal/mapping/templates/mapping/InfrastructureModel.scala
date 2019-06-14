package com.claudiacauli.www.cloudformal.mapping.templates.mapping

import java.io.File

import aws.cfn.model.{Model, ModelIRI, ModelWriter}
import aws.cfn.mapping.templates.Infrastructure
import com.claudiacauli.www.cloudformal.model.{Model, ModelIRI, ModelWriter}
import com.typesafe.scalalogging.StrictLogging
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model._


object InfrastructureModel
{
  def fromInfrastructure(infrastructure: Infrastructure): Model =
    InfrastructureModelMapper.encode(infrastructure)

}


private class InfrastructureModel
(val stacksetsModels: Set[StackSetModel], val infrastructure:Infrastructure)
  extends Model with StrictLogging
{

  val name
  : String = infrastructure.name
  val manager
  : OWLOntologyManager = OWLManager.createOWLOntologyManager()
  val ontology
  : OWLOntology = manager.createOntology(ModelIRI.infrastructureModelIRI(name))
  val df
  : OWLDataFactory = manager.getOWLDataFactory



  def writeToOutputFolder(destinationFolder: String): Unit =
  {
    val outputDir = withTrailingSlash(destinationFolder)
    val dir = new File(outputDir+name)
    if (!dir.exists) dir.mkdir

    stacksetsModels
      .foreach(_.writeToOutputFolder(outputDir+name+"/")
//        ModelWriter.writeStackSetToFolder(
//          ssM,
//          outputDir+name+"/")
      )

    stackSetFilesInSubdirs(dir)
      .foreach(
        f => importFile(this,f))

    logger.info(s"Writing InfrastructureModel to file.")

    ModelWriter
      .writeInfrastructureToOutputFolder(this,outputDir)
  }


  private def withTrailingSlash(str: String) =
    str + (if (!str.endsWith("/")) "/" else "")




}
