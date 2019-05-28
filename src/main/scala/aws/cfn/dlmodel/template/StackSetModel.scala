package aws.cfn.dlmodel.template

import aws.cfn.dlmodel.{DLModel, DLModelWriter, DLModelIRI}
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{AddImport, OWLDataFactory, OWLOntology, OWLOntologyManager}

class StackSetModel (val name: String, val ontologies: Vector[OWLOntology]) extends DLModel {

  val manager :OWLOntologyManager = OWLManager.createOWLOntologyManager()
  val ontology: OWLOntology = manager.createOntology(DLModelIRI.stackSetIRI(name))
  val df: OWLDataFactory = manager.getOWLDataFactory

  ontologies.foreach(o => importOntology(this,o))


  def pruneToFlow() : DataFlowModel = {
    val dfModel = new DataFlowModel(name)
    copyImportsFromTo(this,dfModel)

    // TODO
    // Do the pruning to preserve only the paths that satisfy a certain property.
    // I work with a forest of tree.
    // For each resource, prune that resource
    // preserve paths that end in another resource
    // drop the paths that end either in subproperty nodes without children or that end in value nodes
    // drop all the subproperty nodes for which either (there is no children) OR (for all the children they end in value nodes)
    // How do people usually prune graphs?


    dfModel
  }


  private def copyImportsFromTo(fromModel:DLModel, toModel:DLModel) = {
    (fromModel.manager.ontologies() filter ( o => o != ontology)) forEach ( o => importOntology(toModel,o))
  }


  private def importOntology(model: DLModel, o: OWLOntology) = {
    model.manager.loadOntology( o.getOWLOntologyManager.getOntologyDocumentIRI(o) )
    model.manager.applyChange( new AddImport( model.ontology, model.df.getOWLImportsDeclaration( o.getOWLOntologyManager.getOntologyDocumentIRI(o) ) ))
  }


  def writeToOutputFolder (destinationFolder: String) = {
    DLModelWriter.writeStackSetToOutputFolder(this,destinationFolder,"")
  }

}