package aws.cfn.actions

import aws.cfn.dlmodel.{Model, ModelIRI, ModelWriter}
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{OWLDataFactory, OWLOntology, OWLOntologyManager}


object ActionsModel
{


  def getFromServiceName(serviceName: String): Set[ActionsModel] = {
    ActionsMapper.fromServiceName(serviceName)
      .map( sA =>
        ActionsModelMapper.fromServiceActions(sA))
  }


}


private[actions]
class ActionsModel(val name:String)
  extends Model
{

  val manager: OWLOntologyManager = OWLManager.createOWLOntologyManager()
  val ontology: OWLOntology = manager.createOntology(ModelIRI.actionsOntologyIRI(name))
  val df: OWLDataFactory = manager.getOWLDataFactory


  def writeToOutputFolder (destinationFolder: String): Unit = {
    ModelWriter
      .writeSpecificationToFolder(
        model = this,outputDir = destinationFolder)
  }


}
