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

import java.io.{File, FileOutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util.UUID.randomUUID

import com.cloud.formal.{Extension, OntologySuffix}
import com.typesafe.scalalogging.StrictLogging
import org.semanticweb.owlapi.formats._
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.util.AutoIRIMapper

import scala.jdk.StreamConverters._

object ModelWriter extends StrictLogging{


    def writeSpecificationToFolder
    (model:Model, outputDir: String, format: String = Format.DefaultFormat ): Unit =
        model.manager
          .saveOntology(
              model.ontology,
              documentFormat(format),
              new FileOutputStream(outputDir+"/"+model.name + Extension.Owl))


    def writeStackSetToFolder
    (model:Model, outputDir: String, format: String = Format.DefaultFormat): Unit = {

        val folderName = outputDir+model.name
        val fileName    = folderName +"/"+ model.name + ModelFileSuffix.StackSet

        makeDirIfDoesNotExist(folderName)

        onlyImportedOntologies(model)
          .foreach (o => {
              saveToOutputFolder(o,model,folderName,format)
              updateImportPointer(o,model,folderName)
        })

        model.manager
          .saveOntology(
              model.ontology,
              documentFormat(format),
              new FileOutputStream(fileName))

        addProtegeCatalogue(
            onlyImportedOntologies(model),
            model.name,
            folderName)
    }


    def writeInfrastructureToOutputFolder
    (model:Model, outputDir:String, format:String = Format.DefaultFormat): Unit = {

        val folderName = outputDir + model.name
        val fileName = folderName +"/"+ model.name + ModelFileSuffix.Infrastructure

        makeDirIfDoesNotExist(folderName)
        model.manager
          .saveOntology(
              model.ontology,
              documentFormat(format),
              new FileOutputStream(fileName))

        logger.info(s"Written InfrastructureModel to file.")
        addProtegeCatalogue(importedStackSets(model),model.name,folderName)
        logger.info(s"Written InfrastructureModel Protege Catalogue to file.")
    }


    def writePermissionToOutputFolder
    (model:Model, outputDir:String, format:String = Format.DefaultFormat ): Unit = {

        val folderName = outputDir+model.name
        val fileName = folderName + "/" + model.name + ModelFileSuffix.Permissions

        makeDirIfDoesNotExist(folderName)

        model.manager
          .saveOntology(
              model.ontology,
              documentFormat(format),
              new FileOutputStream(fileName))
        logger.info("Written PermissionsModel to file.")
    }





    private def importedStackSets(model:Model) =
        onlyImportedOntologies(model) filter isStackSetOntology


    private def isStackSetOntology(ontology : OWLOntology) =
        ontology.getOntologyID
          .getOntologyIRI.get().toString
          .endsWith( OntologySuffix.StackSet + "#")


    private def onlyImportedOntologies(model: Model) =
        model.manager
          .ontologies().toScala(List)
          .filter (o => o!=model.ontology )


    private def addProtegeCatalogue
    (modelImportedOntologies: List[OWLOntology], modelName:String, outputDir : String): Unit = {

        val CatalogueHeader     =
            "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n"
        val CatalogueTrailer    =
            "</catalog>"
        val ImportEntryHeader   =
            "<catalog prefer=\"public\" xmlns=\"urn:oasis:names:tc:entity:xmlns:xml:catalog\">"
        val ImportBlockHeader   = CatalogueHeader + ImportEntryHeader



        def importEntries = {

            def importEntryLine (o : OWLOntology)  = {

                val oIRI =
                    o.getOntologyID.getOntologyIRI.get.toString
                val oName = oIRI.split("/").last.dropRight(1)
                val oPath =
                    if(oName.contains(OntologySuffix.StackSet))
                        oName.split("_").head + "/" +
                          oName.split("_").head + ModelFileSuffix.StackSet
                    else oName + Extension.Owl

                "\n    <uri id=\""+ randomUUID +"\" name=\"" +
                  oIRI + "\" uri=\"" + oPath +"\"/>" +
                  (if (oName.contains(OntologySuffix.StackSet))
                    "\n    <nextCatalog catalog=\"" + oName.split("_").head +
                      "/catalog-v001.xml"+"\"/>"
                    else
                      "")
            }

            modelImportedOntologies
              .foldLeft (ImportBlockHeader)( (acc,o) =>
                  acc + importEntryLine(o))
        }


        val protegeCatalogueString = importEntries + "\n" + CatalogueTrailer + "\n"


        Files.write(
            Paths.get(outputDir + "/" + ProtegeCatalogue.FileName),
            protegeCatalogueString.getBytes(StandardCharsets.UTF_8)
        )
    }


    private def saveToOutputFolder(ontology: OWLOntology, m: Model,
                                   folder: String, format: String): Unit = {
        m.manager
          .saveOntology( ontology, documentFormat(format),
              new FileOutputStream(
                  folder+"/"+ ontology.getOWLOntologyManager
                    .getOntologyDocumentIRI(ontology).toString.split("/").last ))
    }


    private def updateImportPointer(ontology: OWLOntology,
                                    m: Model, folder: String): Unit = {
        val oldDocIRI = m.manager.getOntologyDocumentIRI(ontology)
        val fileName = ontology.getOntologyID.
          getOntologyIRI.toString.
          split("/").last.dropRight(2) + Extension.Owl
        val newDocIRI = IRI.create(
            "file:" + folder + "/" + fileName)
        val oIRI  = ontology.getOntologyID.getOntologyIRI.get()

        m.manager.applyChange(
            new RemoveImport(
                m.ontology,
                m.df.getOWLImportsDeclaration(oldDocIRI)))

        m.manager.setOntologyDocumentIRI(ontology,newDocIRI)

        m.manager.getIRIMappers.add(new AutoIRIMapper(new File(folder),false))
        m.manager.loadOntologyFromOntologyDocument(new File(folder+"/"+fileName))
        m.manager.applyChange(
            new AddImport(m.ontology,
                m.df.getOWLImportsDeclaration( oIRI )))
    }


    private def makeDirIfDoesNotExist(folderName:String) = {
        val dir = new File(folderName)
        if (!dir.exists())
            dir.mkdir()
        dir
    }


    private def documentFormat(format: String) =
        format match {
            case Format.Xml         => new OWLXMLDocumentFormat
            case Format.Turtle      => new TurtleDocumentFormat
            case Format.Functional  => new FunctionalSyntaxDocumentFormat
            case _                  => new RDFXMLDocumentFormat
        }


}