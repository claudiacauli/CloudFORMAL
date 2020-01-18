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

import com.cloud.formal.FilePath
import org.semanticweb.owlapi.model.OWLOntologyManager

private class StackSet( val name:String,
                val infrastructure: Infrastructure,
                val manager: OWLOntologyManager) {

  private[templates] var templates: Vector[Template] = Vector()
  private[templates] var foreignNodes: Map[String,ExternalResource] = Map()

  manager
    .loadOntologyFromOntologyDocument(new File(FilePath.AwsOntology))

  override def toString: String = {
    "\tStackSet: " + name + ", includes Templates: " + "\n" +
      templates.foldLeft("")((a,b)=> a + b.toString + "\n")
  }

  private[templates]
  def getResourcesCount =
    templates.foldLeft(0)((a,t) => a + t.resources.size)

  private[templates]
  def getResourceTypes =
    templates.foldLeft(Set[String]())((a,t) =>
      a ++ t.resources.flatMap(rEntry => Set(rEntry._2.resourceType)))

}
