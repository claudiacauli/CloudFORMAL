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

package com.cloud.formal.model

import java.io.File

import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.model.parameters.ChangeApplied
import org.semanticweb.owlapi.util.AutoIRIMapper


trait Model {


  private[model] val name      : String
  private[model] val manager   : OWLOntologyManager
  private[model] val df        : OWLDataFactory
  private[model] val ontology  : OWLOntology


  def writeToOutputFolder (destinationFolder: String)


  def importOntology(model: Model, o: OWLOntology): Unit =
    importDocIRI(
      model,
      o.getOWLOntologyManager.getOntologyDocumentIRI(o))


  private def importDocIRI(model: Model, docIRI: IRI) = {
    try {
      model.manager.loadOntology(docIRI)
      model.manager.applyChange(
        new AddImport(
          model.ontology,model.df.getOWLImportsDeclaration(docIRI)))
    } catch {
      case e:OWLOntologyDocumentAlreadyExistsException => e.getMessage
    }
  }


  def importFile(model: Model, owlFile: File): ChangeApplied = {
    val importFolder = owlFile.getAbsolutePath.split(owlFile.getName)(0)
    model.manager.getIRIMappers.add(
      new AutoIRIMapper(new File(importFolder),true)
    )
    val o = model.manager.loadOntologyFromOntologyDocument(owlFile)
    model.manager.applyChange(
      new AddImport(
        model.ontology,
        model.df.getOWLImportsDeclaration(
          o.getOntologyID.getOntologyIRI.get))

    )
  }


  protected def stackSetFilesInSubdirs(currentDir: File): Vector[File] = {
    currentDir.listFiles()
      .filter(_.isDirectory).toVector
      .flatMap(_.listFiles().toVector
        .filter (_.getName.endsWith(ModelFileSuffix.StackSet)))
  }


}
