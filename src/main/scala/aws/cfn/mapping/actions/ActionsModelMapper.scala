package aws.cfn.mapping.actions

import aws.cfn.model.ModelIRI
import aws.cfn.mapping.Renaming

import scala.jdk.CollectionConverters._


private object ActionsModelMapper
{


  def fromServiceActions(serviceActions: ServiceActions): ActionsModel =
    new ActionsModelMapper(serviceActions).encode()


}


private
class ActionsModelMapper(val serviceActions: ServiceActions)
{

  private val AssumeRoleService = "sts"
  private val AssumeRoleAction  = "assumerole"
  private val m = new ActionsModel(Renaming.ServActName(serviceActions.service))
  private val assumeRoleProp = m.df
    .getOWLObjectProperty(ModelIRI.actionIRI(AssumeRoleService,AssumeRoleAction))


  def encode(): ActionsModel = {
    m.ontology.add(
      serviceActions.actions
        .flatMap(actionToRole)
        .asJava)
    m
  }


  private def actionToRole(act : Action)  = {
    val actionProp =  m.df.getOWLObjectProperty(
      ModelIRI.actionIRI(serviceActions.service,act.name))

    Vector(m.df.getOWLDeclarationAxiom(actionProp)) ++
      Vector(m.df.getOWLSubPropertyChainOfAxiom(
        List(assumeRoleProp,actionProp).asJava,
        actionProp))
  }


}

