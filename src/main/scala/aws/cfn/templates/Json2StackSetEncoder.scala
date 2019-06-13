package aws.cfn.templates

import java.io.File

import argonaut.Json
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{OWLDataFactory, OWLOntologyManager}


protected object Json2StackSetEncoder {

  // Template ingested as: (Name, TemplateJson, DescriptorJson)
  def encode(iE: Json2InfrastructureEncoder, templates: Vector[(String,Json,Option[Json])], stackSetName: String): StackSet =
    new Json2StackSetEncoder(iE,templates, stackSetName).encode()

}



protected class Json2StackSetEncoder(val iE:Json2InfrastructureEncoder,
                           templates: Vector[(String,Json,Option[Json])],
                           stackSetName: String) {

  val manager: OWLOntologyManager = OWLManager.createOWLOntologyManager()
  val df : OWLDataFactory = manager.getOWLDataFactory
  manager.loadOntologyFromOntologyDocument(
    new File("src/main/resources/terminology/aws.owl"))

  val stackSet = new StackSet(stackSetName, iE.infrastructure, manager)

  val templatesEncoders: Vector[Json2TemplateEncoder] =
    templates map (t => new Json2TemplateEncoder(this,t._1,t._2, t._3))

  val outputsByExportName : Map[String,Node] =
    (templatesEncoders flatMap ( te => te.outputByExportName )).toMap

  val resourceByArn: Map[String,Node] = Map()
    //(templatesEncoders flatMap (te => te.resourceByArn)).toMap

  //val resourcesByAccount : Map[String,Vector[Node]] = (templatesEncoders flatMap () )

  var foreignResourcesByArn : Map[String,ExternalResource] = Map() // TODO




  def updateResourcesNames() : Unit = {
    templatesEncoders foreach (tE => tE.updateResourcesNames())
  }

  def encode(): StackSet = {
    stackSet.templates    = templatesEncoders map ( te => te.encode() )
    stackSet.foreignNodes = foreignResourcesByArn
    stackSet
  }

  def encodePolicies() : StackSet = {
    templatesEncoders foreach ( tE => tE.policyEncoders foreach (pE => pE.encode()) )
    stackSet
  }

}