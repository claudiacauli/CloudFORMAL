package aws.cfn.model

import java.io.{File, FileOutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import aws.cfn.{Extension, OntologySuffix}
import org.semanticweb.owlapi.formats._
import org.semanticweb.owlapi.model._

import scala.jdk.StreamConverters._

object ModelWriter {


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
            updateImportPointer(o,model,folderName)
            saveToOutputFolder(o,model,folderName,format)
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

        addProtegeCatalogue(importedStackSets(model),model.name,folderName)
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
          .filter ( o => o!=model.ontology )


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
                    else oName

                "\n    <uri id=\"Imports Wizard Entry\" name=\"" +
                  oIRI + "\" uri=\"" + oPath +"\"/>"
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
        val newDocIRI = IRI.create(
            "file:" + folder + "/" +
              ontology.getOntologyID.getOntologyIRI.toString.split("/").last.dropRight(2)
              + Extension.Owl)

        m.manager.applyChange(
            new RemoveImport(
                ontology,
                m.df.getOWLImportsDeclaration(oldDocIRI)))

        m.manager.setOntologyDocumentIRI(ontology,newDocIRI)

        m.manager.applyChange(
            new AddImport(ontology,
                m.df.getOWLImportsDeclaration( newDocIRI )))
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