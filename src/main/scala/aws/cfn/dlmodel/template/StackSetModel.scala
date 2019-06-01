package aws.cfn.dlmodel.template

import aws.cfn.dlmodel.{DLModel, DLModelIRI, DLModelWriter}
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model._

class StackSetModel (val name: String, val ontologies: Vector[OWLOntology]) extends DLModel {

  val manager :OWLOntologyManager = OWLManager.createOWLOntologyManager()
  val ontology: OWLOntology = manager.createOntology(DLModelIRI.stackSetIRI(name))
  val df: OWLDataFactory = manager.getOWLDataFactory

  ontologies.foreach(o => importOntology(this,o))


  def pruneToDataFlowModel() : DataFlowModel = {
    val dfModel = new DataFlowModel(name)
    copyImportsFromTo(this,dfModel)
    copyResourcesFromTo(this,dfModel)
    resourcesInModel(this) forEach( r => copyOnlyPropertyPathsLeadingToAnotherResource(r,dfModel) )
    dfModel
  }

  def writeToOutputFolder (destinationFolder: String): Unit = {
    DLModelWriter.writeStackSetToOutputFolder(this,destinationFolder,"")
  }



  private def copyOnlyPropertyPathsLeadingToAnotherResource(i: OWLNamedIndividual, toModel:DLModel) : OWLNamedIndividual = {
    copyAssertions(i, ontology.objectPropertyAssertionAxioms(i) filter (ax=>leadToResource(ax)),toModel)
    i
  }

  private def copyAssertions(i:OWLNamedIndividual, assertions: java.util.stream.Stream[OWLObjectPropertyAssertionAxiom], toModel:DLModel) = {
    assertions forEach (
      edge => toModel.ontology.add(toModel.df.getOWLObjectPropertyAssertionAxiom(edge.getProperty,i,
        if (isResource(edge.getObject.asOWLNamedIndividual())) edge.getObject.asOWLNamedIndividual()
        else copyOnlyPropertyPathsLeadingToAnotherResource(edge.getObject.asOWLNamedIndividual(),toModel))
      ))
    i
  }

  private def leadToResource(subproperty: OWLNamedIndividual) : Boolean = {
    ontology.objectPropertyAssertionAxioms(subproperty) anyMatch (edge => leadToResource(edge))
  }

  private def leadToResource(propertyEdge: OWLObjectPropertyAssertionAxiom) : Boolean = {
    val targetIndividual = propertyEdge.getObject.asOWLNamedIndividual()
    isResource(targetIndividual) || leadToResource(targetIndividual)
  }

  private def copyResourcesFromTo(fromModel: DLModel, toModel: DLModel) = {
    resourcesInModel(fromModel) forEach(r=> copyResourceFromTo(r, fromModel,toModel))
  }

  private def copyResourceFromTo(resource:OWLNamedIndividual, fromModel: DLModel, toModel: DLModel) = {
    val name = resource.getIRI.toString.split("#").last
    val concept =
      if (fromModel.ontology.classAssertionAxioms(resource).findFirst().isPresent)
        Some(fromModel.ontology.classAssertionAxioms(resource).findFirst().get().getClassExpression)
      else None

    val copiedResource = toModel.df.getOWLNamedIndividual(DLModelIRI.resourceInstanceIRI(toModel.name,name) )

    concept match {
      case None => toModel.ontology.add(
        toModel.df.getOWLDeclarationAxiom(copiedResource))
      case Some(c) => toModel.ontology.add(toModel.df.getOWLClassAssertionAxiom(c,copiedResource))
    }
  }


  private def resourcesInModel(m:DLModel)=
    m.ontology.individualsInSignature() filter (i=>isResource(i))


  private def isResource(i: OWLNamedIndividual) : Boolean = hasComment(i,"resource")

  private def isSubproperty(i:OWLNamedIndividual) = hasComment(i,"subproperty")

  private def isPolicy(i:OWLNamedIndividual) = hasComment(i,"policy")

  private def hasComment(i:OWLNamedIndividual, comment:String) = {
    if ( ontology.annotationAssertionAxioms(i.getIRI).findAny().isPresent ){
      ontology.annotationAssertionAxioms(i.getIRI).findFirst().get().annotationValue().equals(df.getOWLLiteral(comment))
    }
    else false
  }

  private def copyImportsFromTo(fromModel:DLModel, toModel:DLModel) = {
    (fromModel.manager.ontologies() filter ( o => o != ontology)) forEach ( o => importOntology(toModel,o))
  }





}