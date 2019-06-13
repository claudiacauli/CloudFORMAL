package aws.cfn.specifications.encoding

import aws.cfn.dlmodel.DLModelIRI
import aws.cfn.dlmodel.specification.ServiceActionsModel
import aws.cfn.shared.SpecificationRenaming
import aws.cfn.specifications.formalization.{Action, ServiceActions}
import org.semanticweb.owlapi.model.{IRI, OWLObjectProperty, OWLObjectPropertyExpression}

import scala.jdk.CollectionConverters._



object ServiceActions2DLEncoder{

  def encode(serviceActionsVector: Set[ServiceActions]) : Set[ServiceActionsModel] =
    serviceActionsVector map ( sA => new ServiceActions2DLEncoder(sA).encode() )

}


private class ServiceActions2DLEncoder(val serviceActions : ServiceActions) {

  val m: ServiceActionsModel = new ServiceActionsModel(SpecificationRenaming.serviceActionsName(serviceActions.serviceName))
  val assumeRoleProp : OWLObjectProperty = m.df.getOWLObjectProperty(DLModelIRI.actionIRI("sts","assumerole"))

  def encode(): ServiceActionsModel = {

    def actionToRole(act : Action)  = {
      val a =  m.df.getOWLObjectProperty( DLModelIRI.actionIRI(serviceActions.serviceName,act.name) )
      Vector( m.df.getOWLDeclarationAxiom( a )) ++
      Vector( m.df.getOWLSubPropertyChainOfAxiom( List(assumeRoleProp,a).asJava, a ) )
    }

    m.ontology.add( (serviceActions.actions flatMap actionToRole).asJava )
    m
  }

}