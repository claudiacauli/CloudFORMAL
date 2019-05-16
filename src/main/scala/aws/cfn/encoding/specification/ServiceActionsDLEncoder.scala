package aws.cfn.encoding.specification

import aws.cfn.dlmodel.Symbols
import aws.cfn.dlmodel.specification.ServiceActionsModel
import aws.cfn.formalization.{Action, ServiceActions}

import scala.jdk.CollectionConverters._



object ServiceActionsDLEncoder{

  def encode(serviceActions: ServiceActions) : ServiceActionsModel =
    new ServiceActionsDLEncoder(serviceActions).encode()

}







class ServiceActionsDLEncoder(val serviceActions : ServiceActions) {

  val m: ServiceActionsModel = new ServiceActionsModel(serviceActions.serviceName+"Actions")

  def encode(): ServiceActionsModel = {

    def actionToRole(act : Action)  = {
      val a =  m.df.getOWLObjectProperty( Symbols.actionIRI(serviceActions.serviceName+"Actions",act.name) )
      Vector( m.df.getOWLDeclarationAxiom( a )) ++
      Vector( m.df.getOWLObjectPropertyRangeAxiom(a, m.df.getOWLClass( Symbols.resourceTypeIRI(serviceActions.serviceName+act.resource, act.resource))) )
    }

    m.ontology.add( (serviceActions.actions flatMap actionToRole).asJava )
    m
  }

}
