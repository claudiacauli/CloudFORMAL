package aws.cfn.dlmodel

import java.io.{File, FileOutputStream, FileWriter, PrintWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import org.semanticweb.owlapi.formats._
import org.semanticweb.owlapi.model.{AddImport, IRI, OWLOntology, RemoveImport}

import scala.jdk.StreamConverters._

object DLModelWriter {

    val stackSetFileSuffix          = "_StackSetModel.owl"
    val stacksetOntoSuffix: String = stackSetFileSuffix.split(".owl").head
    val infrastructureFileSuffix    = "_InfrastructureModel.owl"
    val dataFlowFileSuffix = "_DataFlowModel.owl"






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
        writeStackSetToOutputFolder(model, model.name+"/", "",format)

    def writeStackSetToOutputFolder(model:DLModel, outputDir: String, modelType : String, format: String = "rdf"): Unit = {

        val folderName = outputDir+model.name
        val fileName = folderName + "/" + model.name +   modelType + stackSetFileSuffix

        def saveToOutputFolder(ontology : OWLOntology): Unit = {
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

        addProtegeCatalogue(onlyImportedOntologies(model),model.name,folderName, withGroupBlock = true)


    }







    def writeInfrastructureToOutputFolder(model:DLModel, outputDir:String, format:String = "rdf"): Unit = {

        val folderName = outputDir+model.name
        val fileName = folderName + "/" + model.name + infrastructureFileSuffix

        makeDirIfDoesNotExist(folderName)

        format.toLowerCase() match {
            case "rdf" => model.manager.saveOntology(model.ontology, new RDFXMLDocumentFormat, new FileOutputStream(fileName))
            case "xml" => model.manager.saveOntology(model.ontology, new OWLXMLDocumentFormat, new FileOutputStream(fileName))
            case "ttl" => model.manager.saveOntology(model.ontology, new TurtleDocumentFormat, new FileOutputStream(fileName))
            case "fun" => model.manager.saveOntology(model.ontology, new FunctionalSyntaxDocumentFormat, new FileOutputStream(fileName))
        }

        addProtegeCatalogue(importedStackSets(model),model.name,folderName)
    }


    def writeDataFlowToOutputFolder(model:DLModel, outputDir:String, format:String = "rdf"): Unit = {

        val folderName = outputDir+model.name
        val fileName = folderName + "/" + model.name + dataFlowFileSuffix

        makeDirIfDoesNotExist(folderName)

        format.toLowerCase() match {
            case "rdf" => model.manager.saveOntology(model.ontology, new RDFXMLDocumentFormat, new FileOutputStream(fileName))
            case "xml" => model.manager.saveOntology(model.ontology, new OWLXMLDocumentFormat, new FileOutputStream(fileName))
            case "ttl" => model.manager.saveOntology(model.ontology, new TurtleDocumentFormat, new FileOutputStream(fileName))
            case "fun" => model.manager.saveOntology(model.ontology, new FunctionalSyntaxDocumentFormat, new FileOutputStream(fileName))
        }

        //  addProtegeCatalogue(importedStackSets(model),model.name,folderName)
    }


    private def importedStackSets(model:DLModel) = {
        onlyImportedOntologies(model) filter isStackSetOntology
    }

    private def isStackSetOntology(ontology : OWLOntology) : Boolean = {
        ontology.getOntologyID.getOntologyIRI.get().toString.endsWith("_stackset#")
    }


    private def onlyImportedOntologies(model: DLModel)
    = model.manager.ontologies().toScala(List) filter ( o => o!=model.ontology)







    private def addProtegeCatalogue(modelImportedOntologies: List[OWLOntology], modelName:String, outputDir : String, withGroupBlock:Boolean = false) : Unit = {

        val header:String = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>"
        val trailer :String = "</catalog>"

        def importEntries: String = {

            val importEntryHeader : String = "<catalog prefer=\"public\" xmlns=\"urn:oasis:names:tc:entity:xmlns:xml:catalog\">"

            def importEntryLine(o : OWLOntology) : String = {
                val ontologyDocumentIRI = o.getOWLOntologyManager.getOntologyDocumentIRI(o).toString
                val ontologyName = ontologyDocumentIRI.split("/").last.split(".owl").head
                val ontologyFolderName = if(ontologyName.contains(stacksetOntoSuffix)) ontologyName.split("_")(0) else ""
                val ontologyPath = if(ontologyName.contains(stacksetOntoSuffix)) ontologyFolderName + "/" + ontologyName else ontologyName
                val ontologyIRI = DLModelIRI.resourceTerminologyIRI(ontologyName)
                "    <uri id=\"Imports Wizard Entry\" name=\"" + ontologyIRI + "\" uri=\"" + ontologyPath +".owl\"/>"
            }

            modelImportedOntologies.foldLeft(header+"\n"+importEntryHeader)( (acc,o) => acc + "\n"  + importEntryLine(o) )

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

            val mainOntoLine = "        <uri id=\"Automatically generated entry, Timestamp=1562608576686\" name=\""+ DLModelIRI.resourceTerminologyIRI(modelName).toString.replaceAll("#","") +"\" uri=\""+ modelName +".owl\"/>"
              modelImportedOntologies.foldLeft(groupEntryHeader+"\n"+mainOntoLine)((acc, o)=>acc+"\n"+groupEntryLine(o))+"\n"+groupEntryTrailer
        }

        val groupBlock = if (withGroupBlock) groupEntries + "\n" else ""
        Files.write(Paths.get(outputDir + "/catalog-v001.xml"), (importEntries + "\n" + groupBlock + trailer+"\n").getBytes(StandardCharsets.UTF_8))

    }






    def makeDirIfDoesNotExist(folderName:String): File = {
        val dir = new File(folderName)
        if (!dir.exists())
            dir.mkdir()
        dir
    }






}