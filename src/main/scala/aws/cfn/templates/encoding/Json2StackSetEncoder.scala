package aws.cfn.templates.encoding

import argonaut.Json
import aws.cfn.templates.formalization.{ForeignResource, Node, StackSet}
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{OWLDataFactory, OWLOntologyManager}


object Json2StackSetEncoder {

  // Template ingested as: (Name, TemplateJson, DescriptorJson)
  def encode(iE: Json2InfrastructureEncoder, templates: Vector[(String,Json,Option[Json])], stackSetName: String): StackSet =
    new Json2StackSetEncoder(iE,templates, stackSetName).encode()

}



class Json2StackSetEncoder(iE:Json2InfrastructureEncoder, templates: Vector[(String,Json,Option[Json])], stackSetName: String) {

  val manager: OWLOntologyManager = OWLManager.createOWLOntologyManager()
  val df : OWLDataFactory = manager.getOWLDataFactory

  val stackSet = new StackSet(stackSetName, manager)

  val templatesEncoders: Vector[Json2TemplateEncoder] =
    templates map (t => new Json2TemplateEncoder(iE,this,t._1,t._2, t._3))

  val outputsByExportName : Map[String,Node] =
    (templatesEncoders flatMap ( te => te.outputByExportName )).toMap

  val resourceByArn: Map[String,Node] =
    (templatesEncoders flatMap (te => te.resourceByArn)).toMap

  //val resourcesByAccount : Map[String,Vector[Node]] = (templatesEncoders flatMap () )

  var foreignResourcesByArn : Map[String,ForeignResource] = Map() // TODO




  def updateResourcesNames() : Unit = {
    templatesEncoders foreach (tE => tE.updateResourcesNames)
  }

  def encode(): StackSet = {
    stackSet.templates = templatesEncoders map ( te => te.encode() )
    stackSet.foreignNodes = foreignResourcesByArn
    stackSet
  }

  override def toString: String = {
    "\tStackSet: " + stackSetName + ", includes Templates: " + "\n" +
    templatesEncoders.foldLeft("")((a,b)=> a + b.toString + "\n")

  }

}