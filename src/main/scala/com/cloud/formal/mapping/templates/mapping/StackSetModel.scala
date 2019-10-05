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

import com.cloud.formal.model.{Model, ModelIRI, ModelWriter}
import com.typesafe.scalalogging.StrictLogging
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model._

class StackSetModel
(val name: String, val ontologies: Vector[OWLOntology])
  extends Model with StrictLogging
{

  val manager :OWLOntologyManager
  = OWLManager.createOWLOntologyManager()
  val ontology: OWLOntology
  = manager.createOntology(ModelIRI.stackSetIRI(name))
  val df: OWLDataFactory
  = manager.getOWLDataFactory

  ontologies
    .foreach(o =>
      importOntology(this,o))



  def writeToOutputFolder(destinationFolder: String): Unit = {
    logger.info(s"Writing $name StackSetModel and its " +
      s"imports to corresponding subfolder.")
    ModelWriter
      .writeStackSetToFolder(this,
        destinationFolder)
  }




}