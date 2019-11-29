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

package com.cloud.formal.mapping.templates.mapping

import java.io.File

import com.cloud.formal.mapping.templates.Infrastructure
import com.cloud.formal.model.{Model, ModelIRI, ModelWriter}
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
      .foreach(_.writeToOutputFolder(outputDir+name+"/"))

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
