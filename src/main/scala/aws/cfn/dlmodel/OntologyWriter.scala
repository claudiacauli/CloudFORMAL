package aws.cfn.dlmodel

import java.io.FileOutputStream

import aws.cfn.dlmodel.terminology.Model
import org.semanticweb.owlapi.formats._

object OntologyWriter {


    def write(model:Model, format: String = "rdf"): Unit =
        writeToOutputDir(model, "", format)

    def writeToOutputDir(model:Model, outputDir: String, format: String = "rdf"): Unit =
        format.toLowerCase() match {
            case "rdf" => model.manager.saveOntology(model.ontology, new RDFXMLDocumentFormat, new FileOutputStream(outputDir+model.name+".owl"))
            case "xml" => model.manager.saveOntology(model.ontology, new OWLXMLDocumentFormat, new FileOutputStream(outputDir+model.name+".owl"))
            case "ttl" => model.manager.saveOntology(model.ontology, new TurtleDocumentFormat, new FileOutputStream(outputDir+model.name+".owl"))
            case "fun" => model.manager.saveOntology(model.ontology, new FunctionalSyntaxDocumentFormat, new FileOutputStream(outputDir+model.name+".owl"))
        }


}
