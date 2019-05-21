package aws.cfn.dlmodel

import java.io.{File, FileOutputStream, FileWriter, PrintWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import org.semanticweb.owlapi.formats._
import org.semanticweb.owlapi.model.OWLOntology

import scala.jdk.StreamConverters._

object OntologyWriter {


    def write(model:DescriptionLogicModel, format: String = "rdf"): Unit =
        writeToOutputDir(model, model.name+"/", format)

    def writeToOutputDir(model:DescriptionLogicModel, outputDir: String, format: String = "rdf"): Unit = {
        mkdir(outputDir+model.name)
        format.toLowerCase() match {
            case "rdf" => model.manager.saveOntology(model.ontology, new RDFXMLDocumentFormat, new FileOutputStream(outputDir+model.name+"/"+model.name+".owl"))
            case "xml" => model.manager.saveOntology(model.ontology, new OWLXMLDocumentFormat, new FileOutputStream(outputDir+model.name+"/"+model.name+".owl"))
            case "ttl" => model.manager.saveOntology(model.ontology, new TurtleDocumentFormat, new FileOutputStream(outputDir+model.name+"/"+model.name+".owl"))
            case "fun" => model.manager.saveOntology(model.ontology, new FunctionalSyntaxDocumentFormat, new FileOutputStream(outputDir+model.name+"/"+model.name+".owl"))
        }
        onlyImportedOntologies(model) foreach (o => model.manager.saveOntology(o, new RDFXMLDocumentFormat, new FileOutputStream(outputDir+model.name+"/"+
          o.getOWLOntologyManager.getOntologyDocumentIRI(o).toString.split("/").last.split("#").head )))
        addProtegeCatalogue(model,outputDir+model.name)
    }

    private def onlyImportedOntologies(model: DescriptionLogicModel)
    = model.manager.ontologies().toScala(List) filter ( o => o!=model.ontology)

    private def mkdir(outputDir:String): Unit = {
        val dir = new File(outputDir)
        if (!dir.exists())
            dir.mkdir()
    }


    private def addProtegeCatalogue(model: DescriptionLogicModel, outputDir : String ) : Unit = {

        val header:String = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>"
        val trailer :String = "</catalog>"

        def importEntries: String = {

            val importEntryHeader : String = "<catalog prefer=\"public\" xmlns=\"urn:oasis:names:tc:entity:xmlns:xml:catalog\">"

            def importEntryLine(o : OWLOntology) : String = {
                val ontologyDocumentIRI = o.getOWLOntologyManager.getOntologyDocumentIRI(o).toString
                val ontologyName = ontologyDocumentIRI.split("/").last.split(".owl").head
                val ontologyIRI = Symbols.resourceTerminologyIRI(ontologyName)
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
                val ontologyIRI = Symbols.resourceTerminologyIRI(ontologyName).toString
                "        <uri id=\"Automatically generated entry, Timestamp=1562608576686\" name=\""+ ontologyIRI.replaceAll("#","") +"\" uri=\""+ ontologyName +".owl\"/>"
            }

            val mainOntoLine = "        <uri id=\"Automatically generated entry, Timestamp=1562608576686\" name=\""+ Symbols.resourceTerminologyIRI(model.name).toString.replaceAll("#","") +"\" uri=\""+ model.name +".owl\"/>"
              (onlyImportedOntologies(model)).foldLeft(groupEntryHeader+"\n"+mainOntoLine)((acc,o)=>acc+"\n"+groupEntryLine(o))+"\n"+groupEntryTrailer
        }

        Files.write(Paths.get(outputDir + "/catalog-v001.xml"), (importEntries + "\n" + groupEntries + "\n" + trailer+"\n").getBytes(StandardCharsets.UTF_8))
        //        new PrintWriter(outputDir + "/catalog-v001.xml"  ){ write((importEntries + "\n" + groupEntries + "\n" + trailer+"\n").getBytes(StandardCharsets.UTF_8)); close()}

    }


}
