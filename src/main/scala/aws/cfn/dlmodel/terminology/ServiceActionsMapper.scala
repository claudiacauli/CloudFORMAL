package aws.cfn.dlmodel.terminology

import aws.cfn.dlmodel.Symbols
import aws.cfn.types.{Action, ServiceActions}

class ServiceActionsMapper (val servActions : ServiceActions) {

  val m: ServiceActionsModel = new ServiceActionsModel(servActions.serviceName+"Actions")

  def map () : ServiceActionsModel = {

    def actionToRole(act : Action) : Unit = {
      val a = m.df.getOWLObjectProperty( Symbols.actionIRI(servActions.serviceName+"Actions",act.name) )
      val t = m.df.getOWLClass( Symbols.resourceTypeIRI(servActions.serviceName+act.resource, act.resource) )
      m.ontology.add( m.df.getOWLDeclarationAxiom(a) )
      m.ontology.add( m.df.getOWLObjectPropertyRangeAxiom(a,t) )
    }
    servActions.actions foreach actionToRole
    m
  }

}
