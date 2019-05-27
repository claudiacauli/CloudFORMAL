package aws.cfn.specifications.encoding

import aws.cfn.dlmodel.DLModelIRI
import aws.cfn.dlmodel.specification.ServiceActionsModel
import aws.cfn.specifications.formalization.{Action, ServiceActions}

import scala.jdk.CollectionConverters._



object ServiceActions2DLEncoder{

  def encode(serviceActionsVector: Vector[ServiceActions]) : Vector[ServiceActionsModel] =
    serviceActionsVector map ( sA => new ServiceActions2DLEncoder(sA).encode() )

}


private class ServiceActions2DLEncoder(val serviceActions : ServiceActions) {

  val m: ServiceActionsModel = new ServiceActionsModel(serviceActions.serviceName+"Actions")

  def encode(): ServiceActionsModel = {

    def actionToRole(act : Action)  = {
      val a =  m.df.getOWLObjectProperty( DLModelIRI.actionIRI(serviceActions.serviceName+"Actions",act.name) )
      Vector( m.df.getOWLDeclarationAxiom( a )) ++
      Vector( m.df.getOWLObjectPropertyRangeAxiom(a, m.df.getOWLClass( DLModelIRI.resourceTypeIRI(serviceActions.serviceName+act.resource, act.resource))) )
    }

    m.ontology.add( (serviceActions.actions flatMap actionToRole).asJava )
    m
  }

}
