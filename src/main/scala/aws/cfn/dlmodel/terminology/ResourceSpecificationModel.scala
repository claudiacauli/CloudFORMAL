package aws.cfn.dlmodel.terminology

import aws.cfn.dlmodel.Symbols
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{OWLDataFactory, OWLOntology, OWLOntologyManager}

class ResourceSpecificationModel(val name : String) extends Model{

  val manager: OWLOntologyManager = OWLManager.createOWLOntologyManager()
  val ontology: OWLOntology = manager.createOntology(Symbols.resourceTerminologyIRI(name))
  val df: OWLDataFactory = manager.getOWLDataFactory

}
