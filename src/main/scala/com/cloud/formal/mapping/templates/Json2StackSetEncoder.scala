/*
 *    Copyright 2019 Claudia Cauli
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */

package com.cloud.formal.mapping.templates

import java.io.File

import argonaut.Json
import com.cloud.formal.FilePath
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