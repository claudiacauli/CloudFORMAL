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

  val manager :OWLOntologyManager
  = OWLManager.createOWLOntologyManager()
  val ontology: OWLOntology
  = manager.createOntology(ModelIRI.permissionsModelIRI(name))
  val df: OWLDataFactory
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
