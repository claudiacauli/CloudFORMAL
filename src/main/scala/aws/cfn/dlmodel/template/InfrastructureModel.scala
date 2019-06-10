package aws.cfn.dlmodel.template

import java.io.File

import aws.cfn.dlmodel.{DLModel, DLModelIRI, DLModelWriter}
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model._

import scala.jdk.StreamConverters._



class InfrastructureModel(val name:String, stacksetsModels: Vector[StackSetModel]) extends DLModel {

  val manager :OWLOntologyManager = OWLManager.createOWLOntologyManager()
  val ontology: OWLOntology = manager.createOntology(DLModelIRI.infrastructureModelIRI(name))
  val df: OWLDataFactory = manager.getOWLDataFactory



  def writeToOutputFolder (destinationFolder: String): Unit = {

    val dir = DLModelWriter.makeDirIfDoesNotExist(destinationFolder+"/"+name)
    stacksetsModels foreach  (ssM => DLModelWriter.writeStackSetToOutputFolder(ssM,destinationFolder+"/"+name+"/",""))
    stackSetFilesInSubdirs(dir) foreach ( f => importFile(this,f) )
    DLModelWriter.writeInfrastructureToOutputFolder(this,destinationFolder)

  }

  private def stackSetFilesInSubdirs(currentDir: File) = {
    (currentDir.listFiles() filter (f => f.isDirectory)).toVector flatMap (
      f => f.listFiles().toVector filter (f => f.getName.endsWith(DLModelWriter.stackSetFileSuffix))
      )
  }



//  def pruneToDataFlowModel() : DataFlowModel = {
//    val dfModel = new DataFlowModel(name)
//    copyStackSetImportsFromTo(this,dfModel)
//    copyResourcesFromTo(this,dfModel)
//    resourcesInModel(this) foreach ( r => copyOnlyPropertyPathsLeadingToAnotherResource(r._2,dfModel, r._1) )
//    dfModel
//  }
//
//
//
//

//
//
//
//
//  private def copyStackSetImportsFromTo(fromModel:DLModel, toModel:DLModel) = {
//    (stacksetsModels flatMap (ssM =>  (ssM.manager.ontologies() filter(o => o != ssM.ontology)).toArray))
//      .foreach ( o => importOntology(toModel,o.asInstanceOf[OWLOntology]))
//  }
//
//  private def copyResourcesFromTo(fromModel: DLModel, toModel: DLModel): Unit = {
//    resourcesInModel(fromModel) foreach (r=> copyResourceFromTo(r._2, fromModel,toModel))
//  }
//
//  private def resourcesInModel(m:DLModel) : Vector[(OWLOntology,OWLNamedIndividual)] = {
//    val vec = stacksetsModels flatMap ( ssM => ssM.ontology.individualsInSignature()
//      .filter (i => isResource(i,ssM.ontology)).toScala(Vector)
//      .map ( i => (ssM.ontology,i) )
//      )
//    vec.asInstanceOf[Vector[(OWLOntology,OWLNamedIndividual)]]
//  }
//
//  private def copyOnlyPropertyPathsLeadingToAnotherResource(i: OWLNamedIndividual, toModel:DLModel, ontology:OWLOntology) : OWLNamedIndividual = {
//    copyAssertions(i, ontology.objectPropertyAssertionAxioms(i) filter (ax=>leadToResource(ax,ontology)),toModel,ontology)
//    i
//  }
//
//  private def isResource(i: OWLNamedIndividual, o: OWLOntology) : Boolean =
//    hasComment(o,i,"resource")
//
//  private def copyAssertions(i:OWLNamedIndividual, assertions: java.util.stream.Stream[OWLObjectPropertyAssertionAxiom], toModel:DLModel, ontology: OWLOntology) = {
//    assertions forEach (
//      edge => toModel.ontology.add(toModel.df.getOWLObjectPropertyAssertionAxiom(edge.getProperty,i,
//        if (isResource(edge.getObject.asOWLNamedIndividual(),ontology)) edge.getObject.asOWLNamedIndividual()
//        else copyOnlyPropertyPathsLeadingToAnotherResource(edge.getObject.asOWLNamedIndividual(),toModel,ontology))
//      ))
//    i
//  }
//
//
//  private def leadToResource(subproperty: OWLNamedIndividual, ontology:OWLOntology) : Boolean = {
//    ontology.objectPropertyAssertionAxioms(subproperty) anyMatch (edge => leadToResource(edge,ontology))
//  }
//
//  private def leadToResource(propertyEdge: OWLObjectPropertyAssertionAxiom, ontology:OWLOntology) : Boolean = {
//    val targetIndividual = propertyEdge.getObject.asOWLNamedIndividual()
//    isResource(targetIndividual,ontology) || leadToResource(targetIndividual,ontology)
//  }
//
//  private def hasComment(ontology: OWLOntology, i:OWLNamedIndividual, comment:String) = {
//    if ( ontology.annotationAssertionAxioms(i.getIRI).findAny().isPresent ){
//      ontology.annotationAssertionAxioms(i.getIRI).findFirst().get().annotationValue().equals(df.getOWLLiteral(comment))
//    }
//    else false
//  }
//
//  private def copyResourceFromTo(resource:OWLNamedIndividual, fromModel: DLModel, toModel: DLModel) = {
//    val name = resource.getIRI.toString.split("#").last
//    val concept =
//      if (fromModel.ontology.classAssertionAxioms(resource).findFirst().isPresent)
//        Some(fromModel.ontology.classAssertionAxioms(resource).findFirst().get().getClassExpression)
//      else None
//
//    val copiedResource = toModel.df.getOWLNamedIndividual(DLModelIRI.resourceInstanceIRI(toModel.name,name) )
//
//    concept match {
//      case None => toModel.ontology.add(
//        toModel.df.getOWLDeclarationAxiom(copiedResource))
//      case Some(c) => toModel.ontology.add(toModel.df.getOWLClassAssertionAxiom(c,copiedResource))
//    }
//  }


//  private def isSubproperty(i:OWLNamedIndividual) = hasComment(i,"subproperty")
//
//  private def isPolicy(i:OWLNamedIndividual) = hasComment(i,"policy")



}
