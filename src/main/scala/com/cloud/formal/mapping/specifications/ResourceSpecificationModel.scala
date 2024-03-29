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

package com.cloud.formal.mapping.specifications

import java.io.{File, FileNotFoundException}

import argonaut.Parse
import com.cloud.formal.Extension
import com.cloud.formal.mapping.Specification
import com.cloud.formal.model.{Model, ModelIRI, ModelWriter}
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{OWLDataFactory, OWLOntology, OWLOntologyManager}

import scala.io.Source

object ResourceSpecificationModel
{

  def fromResourceSpecification(resSpec: ResourceSpecification): Model =
    ResourceSpecificationModelMapper
      .fromSpecification(resSpec)


  def fromResourceSpecificationFile(f: File): Model = {

    val fileName = f.getName
        .split(Specification.SpecificationTrailer+Extension.JSON)
        .head

      Parse.parseOption(Source.fromFile(f).mkString) match {
        case None     =>
          println("Parsing of Json from file " + f.getName + " failed.")
          sys.exit(0)
        case Some(j)  =>
          fromResourceSpecification(
            ResourceSpecificationMapper.fromJson(j,fileName))
      }
  }

  def fromResourceSpecificationFilePath(filePath: String): Model = {
    try {
      val f = new File(filePath)
      fromResourceSpecificationFile(f)
    } catch {
      case e:FileNotFoundException =>
        throw new FileNotFoundException("A file with path " + filePath + " was not found.\n" + e.getMessage)
    }
  }


}



private class ResourceSpecificationModel
(val name : String)
  extends Model
{

  val manager: OWLOntologyManager = OWLManager.createOWLOntologyManager()
  val ontology: OWLOntology = manager.createOntology(ModelIRI.resourceTerminologyIRI(name))
  val df: OWLDataFactory = manager.getOWLDataFactory

  def writeToOutputFolder (destinationFolder: String): Unit =
    ModelWriter
      .writeSpecificationToFolder(this,destinationFolder)



}
