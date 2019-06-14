package com.claudiacauli.www.cloudformal.mapping.templates

import java.io.File

import argonaut.Json
import aws.cfn.FilePath
import com.typesafe.scalalogging.StrictLogging
import org.semanticweb.owlapi.apibinding.OWLManager


private object Json2StackSetEncoder
{

  // Template ingested as: (Name, TemplateJson, DescriptorJson)
  private[templates]
  def encode(iE: Json2InfrastructureEncoder,
             templates: Vector[(String,Json,Option[Json])],
             stackSetName: String)
  : StackSet =
    new Json2StackSetEncoder(iE,templates, stackSetName).encode()

}



private class Json2StackSetEncoder(val iE:Json2InfrastructureEncoder,
                           templates: Vector[(String,Json,Option[Json])],
                           stackSetName: String)
extends StrictLogging
{

  logger.info(s"Initializing $stackSetName StackSet Encoder")

  private[templates] val manager  = OWLManager.createOWLOntologyManager()
  private[templates] val df       = manager.getOWLDataFactory
  private[templates] val stackSet = new StackSet(stackSetName, iE.infrastructure, manager)
  private[templates] val templatesEncoders    = getTemplateEncoders
  private[templates] val outputsByExportName  = getOutputsByExport
  private[templates] var foreignResourcesByArn : Map[String,ExternalResource] = Map()



  private[templates]
  def updateResourcesNames(): Unit =
    templatesEncoders foreach (_.updateResourcesNames())



  private[templates]
  def encode() = {
    manager.loadOntologyFromOntologyDocument(
      new File(FilePath.AwsOntology))
    stackSet.templates    = templatesEncoders map (_.encode())
    stackSet.foreignNodes = foreignResourcesByArn
    stackSet
  }




  private[templates]
  def encodePolicies() = {
    templatesEncoders
      .foreach(_.policyEncoders
        .foreach (_.encode()))
    stackSet
  }




  private def getTemplateEncoders =
    templates.map(t =>
      new Json2TemplateEncoder(this,t._1,t._2, t._3))

  private def getOutputsByExport =
    templatesEncoders
      .flatMap(_.outputByExportName)
      .toMap



}