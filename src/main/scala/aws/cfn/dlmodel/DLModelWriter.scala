package aws.cfn.dlmodel

import java.io.{File, FileOutputStream, FileWriter, PrintWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import org.semanticweb.owlapi.formats._
import org.semanticweb.owlapi.model.{AddImport, IRI, OWLOntology, RemoveImport}

import scala.jdk.StreamConverters._

package object DLModelWriter {


    def writeSpecification(model:DLModel, format: String = "rdf") : Unit =
        writeSpecificationToOutputFolder(model, model.name+"/", format)


    def writeSpecificationToOutputFolder(model:DLModel, outputDir: String, format: String = "rdf"): Unit = {
        format.toLowerCase() match {
            case "rdf" => model.manager.saveOntology(model.ontology, new RDFXMLDocumentFormat, new FileOutputStream(outputDir+"/"+model.name+".owl"))
            case "xml" => model.manager.saveOntology(model.ontology, new OWLXMLDocumentFormat, new FileOutputStream(outputDir+"/"+model.name+".owl"))
            case "ttl" => model.manager.saveOntology(model.ontology, new TurtleDocumentFormat, new FileOutputStream(outputDir+"/"+model.name+".owl"))
            case "fun" => model.manager.saveOntology(model.ontology, new FunctionalSyntaxDocumentFormat, new FileOutputStream(outputDir+"/"+model.name+".owl"))
        }
    }


    def writeStackSet(model:DLModel, format: String = "rdf"): Unit =
        writeStackSetToOutputFolder(model, model.name+"/", format)

    def writeStackSetToOutputFolder(model:DLModel, outputDir: String, format: String = "rdf"): Unit = {

        val folderName = outputDir+model.name
        val fileName = folderName + "/" + model.name + "_StackSetOWLModel.owl"

        def makeDirIfDoesNotExist(folderName:String): Unit = {
            val dir = new File(folderName)
            if (!dir.exists())
                dir.mkdir()
        }

        def saveToOutputFolder(ontology : OWLOntology) = {
            model.manager.saveOntology(ontology, new RDFXMLDocumentFormat, new FileOutputStream(folderName+"/"+
              ontology.getOWLOntologyManager.getOntologyDocumentIRI(ontology).toString.split("/").last ))
        }

        def updateImportPointer(ontology: OWLOntology) = {
            val oldDocIRI = ontology.getOWLOntologyManager.getOntologyDocumentIRI(ontology)
            val newDocIRI = IRI.create( "file:" + folderName + "/" + ontology.getOWLOntologyManager.getOntologyDocumentIRI(ontology).toString.split("/").last )

            model.manager.applyChange(
                    new RemoveImport(model.ontology, model.df.getOWLImportsDeclaration(oldDocIRI)))
            model.manager.setOntologyDocumentIRI( ontology, newDocIRI )
            model.manager.applyChange( new AddImport(model.ontology, model.df.getOWLImportsDeclaration( ontology.getOWLOntologyManager.getOntologyDocumentIRI(ontology) )))
        }

        makeDirIfDoesNotExist(folderName)
        onlyImportedOntologies(model) foreach (o => {
            updateImportPointer(o)
            saveToOutputFolder(o)
        })
        format.toLowerCase() match {
            case "rdf" => model.manager.saveOntology(model.ontology, new RDFXMLDocumentFormat, new FileOutputStream(fileName))
            case "xml" => model.manager.saveOntology(model.ontology, new OWLXMLDocumentFormat, new FileOutputStream(fileName))
            case "ttl" => model.manager.saveOntology(model.ontology, new TurtleDocumentFormat, new FileOutputStream(fileName))
            case "fun" => model.manager.saveOntology(model.ontology, new FunctionalSyntaxDocumentFormat, new FileOutputStream(fileName))
        }

        addProtegeCatalogue(model,folderName)


    }




    private def onlyImportedOntologies(model: DLModel)
    = model.manager.ontologies().toScala(List) filter ( o => o!=model.ontology)




    private def addProtegeCatalogue(model: DLModel, outputDir : String ) : Unit = {

        val header:String = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>"
        val trailer :String = "</catalog>"

        def importEntries: String = {

            val importEntryHeader : String = "<catalog prefer=\"public\" xmlns=\"urn:oasis:names:tc:entity:xmlns:xml:catalog\">"

            def importEntryLine(o : OWLOntology) : String = {
                val ontologyDocumentIRI = o.getOWLOntologyManager.getOntologyDocumentIRI(o).toString
                val ontologyName = ontologyDocumentIRI.split("/").last.split(".owl").head
                val ontologyIRI = DLModelIRI.resourceTerminologyIRI(ontologyName)
                "    <uri id=\"Imports Wizard Entry\" name=\"" + ontologyIRI + "\" uri=\"" + ontologyName+".owl\"/>"
            }

            onlyImportedOntologies(model).foldLeft(header+"\n"+importEntryHeader)( (acc,o) => acc + "\n"  + importEntryLine(o) )

        }

        def groupEntries: String = {

            val groupEntryHeader : String = "    <group id=\"Folder Repository, directory=, recursive=true, Auto-Update=true, version=2\" prefer=\"public\" xml:base=\"\">"
            val groupEntryTrailer: String = "    </group>"

            def groupEntryLine(o: OWLOntology) : String = {
                val ontologyDocumentIRI = o.getOWLOntologyManager.getOntologyDocumentIRI(o).toString
                val ontologyName = ontologyDocumentIRI.split("/").last.split(".owl").head
                val ontologyIRI = DLModelIRI.resourceTerminologyIRI(ontologyName).toString
                "        <uri id=\"Automatically generated entry, Timestamp=1562608576686\" name=\""+ ontologyIRI.replaceAll("#","") +"\" uri=\""+ ontologyName +".owl\"/>"
            }

            val mainOntoLine = "        <uri id=\"Automatically generated entry, Timestamp=1562608576686\" name=\""+ DLModelIRI.resourceTerminologyIRI(model.name).toString.replaceAll("#","") +"\" uri=\""+ model.name +".owl\"/>"
              (onlyImportedOntologies(model)).foldLeft(groupEntryHeader+"\n"+mainOntoLine)((acc,o)=>acc+"\n"+groupEntryLine(o))+"\n"+groupEntryTrailer
        }

        Files.write(Paths.get(outputDir + "/catalog-v001.xml"), (importEntries + "\n" + groupEntries + "\n" + trailer+"\n").getBytes(StandardCharsets.UTF_8))
        //        new PrintWriter(outputDir + "/catalog-v001.xml"  ){ write((importEntries + "\n" + groupEntries + "\n" + trailer+"\n").getBytes(StandardCharsets.UTF_8)); close()}

    }


}
