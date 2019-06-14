package com.cloud.formal.mapping.templates.mapping

import com.cloud.formal.AwsOntology
import com.cloud.formal.mapping.actions.ActionsMap
import com.cloud.formal.mapping.templates._
import com.cloud.formal.model.{Model, ModelIRI}
import org.semanticweb.owlapi.model._

import scala.jdk.CollectionConverters._
import scala.language.postfixOps

protected object PermissionsModelMapper
{
  def encode(infrastructure: Infrastructure): Model =
    new PermissionsModelMapper(infrastructure)
      .encode()
}


private class PermissionsModelMapper(val infrastructure: Infrastructure)
{

  protected[mapping] val name: String
  = infrastructure.name
  protected[mapping] val statements: Set[Statement]
  = infrastructure.aclStatements
  protected[mapping] val permissionsModel : PermissionsModel
  = new PermissionsModel(name)
  protected[mapping] val df : OWLDataFactory
  = permissionsModel.df


  def encode (): PermissionsModel =
  {
    permissionsModel
      .ontology
      .add(axiomsFromStatements()
        .asJava)
    permissionsModel
  }


  private def axiomsFromStatements() =
    resourceActionPairs flatMap getAclAxiom


  private def getAclAxiom(ra: (Resource,String))  =
  {
    val r = individual(ra._1)
    val a = property(ra._2)

    val explicitlyAllowed = overApproximatedAllowSet(ra._1,ra._2)
    val explicitlyDenied  = underApproximatedDenySet(ra._1,ra._2)

    val allowedMinusDenied =
      df.getOWLObjectIntersectionOf(
        explicitlyAllowed,
        not(explicitlyDenied))

    Set(
      subclassOf(
        allowedMinusDenied,
        performActionOnResource(a,r))
    ) ++
    Set(
      subclassOf(
        explicitlyDenied,
        doNotPerformActionOnResource(a,r)
      )
    )
  }


  private def performActionOnResource(a:OWLObjectProperty, r:OWLNamedIndividual) =
    df.getOWLObjectSomeValuesFrom(
      a,nominalOf(r))


  private def doNotPerformActionOnResource(a:OWLObjectProperty, r:OWLNamedIndividual) =
    df.getOWLObjectComplementOf(
      df.getOWLObjectSomeValuesFrom(
      a,nominalOf(r)))


  private def subclassOf(lhs: OWLClassExpression, rhs:OWLClassExpression) =
    df.getOWLSubClassOfAxiom(
      lhs,rhs)


  private def nominalOf(r:OWLNamedIndividual) =
    df.getOWLObjectOneOf(r)



  private def overApproximatedAllowSet(r:Principal,a:String) = {

    /*
    todo This function is returning always an empty set! Try to understand why!
     */
    def trustedPrincipals(princ:Set[Principal],r:Principal)=
      princ.filter(p =>
        assumeRoleStatements.exists(s =>
          (( s.principals._1 && ( s.principals._2.contains(r) || s.principals._2.contains(Public) ))
            || (!s.principals._1 && ( !s.principals._2.contains(r) && !s.principals._2.contains(Public) )))
            && (
            if (p.isInstanceOf[Resource])
              s.resources._2.toSet.contains(p.asInstanceOf[Resource])
            else true
          ))
      )



    def checkIfAttemptingToAssumeRole(s: Statement) = {
      if (s.actions._2 contains "sts:AssumeRole")
        trustedPrincipals(s.principals._2 ,r)
      else
        s.principals._2
    }



    (allowStatements & statementsWithResource(r) & statementsWithAction(a))
      .foldLeft[OWLClassExpression](df.getOWLNothing)(
        (filler,s) =>
          df.getOWLObjectUnionOf(
            filler,
            actualPrincipals(isAllow = true,checkIfAttemptingToAssumeRole(s)))
    )
  }


  private def underApproximatedDenySet(r:Entity,a: String) =
    (denyStatements & statementsWithResource(r) & statementsWithAction(a))
      .foldLeft[OWLClassExpression](df.getOWLNothing)(
        (filler,s) =>
          df.getOWLObjectUnionOf(
            filler,
            if (s.hasCondition) df.getOWLNothing
            else actualPrincipals(isAllow = false,s.principals._2))
    )


  private def not(ce: OWLClassExpression) =
    df.getOWLObjectComplementOf(ce)


  private def actualPrincipals(isAllow: Boolean, principals:Set[Principal]) =
  {
    val conceptPrincipals =
      principals
        .map(conceptFromPrincipal)
          .foldLeft[OWLClassExpression](
            df.getOWLNothing)(
              (filler,c) =>
                df.getOWLObjectUnionOf(filler,c) )

    if (isAllow)
      conceptPrincipals
    else
      not(conceptPrincipals)
  }


  private def resourceActionPairs() : Set[(Resource,String)] =
  {
    def actionCanBePerformedOverResource(a: String,r: Entity) =
      ActionsMap
        .lookupServiceName(r.asInstanceOf[StackSetResource].serviceType)
          .contains(a)

    def actionIsInStatementsWithEntity(a:String,r:Entity) =
      (statementsWithResource(r) & statementsWithAction(a))
        .nonEmpty

    for { s <- statements
          r <- actualResources(s)
          a <- actualActions(s)
          if  (isStackSetResource(r) && actionCanBePerformedOverResource(a,r)) ||
              (isExternalResource(r) && actionIsInStatementsWithEntity(a,r))
    } yield
      (r,a)
  }


  private def actualResources(statement: Statement)=
  {
    def complementOf(resources: Set[Resource]) =
      (for (s <- statements; r<- s.resources._2) yield r) &~ resources

    if (statement.resources._1)
      statement.resources._2.toSet
    else
      complementOf(statement.resources._2.toSet)
  }


  private def actualActions(statement: Statement) =
  {
    def complementOf(actions : Set[String])=
      (for (s <- statements; a<- s.actions._2) yield a) &~ actions

    if (statement.actions._1)
      statement.actions._2.toSet
    else
      complementOf(statement.actions._2.toSet)
  }


  private def allowStatements =
    statements.filter(_.isInstanceOf[AllowStatement])


  private def denyStatements =
    statements.filter(_.isInstanceOf[DenyStatement])


  private def assumeRoleStatements =
    statements.filter(_.isAssumeRoleStatement)


  private def statementsWithResource(r: Entity)=
    statements.filter(_.resources._2.contains(r))


  private def statementsWithAction(a: String) =
    statements.filter(_.actions._2.contains(a))


  private def isStackSetResource(e: Entity)          =
    e.isInstanceOf[StackSetResource]


  private def isExternalResource(e: Entity)    =
    e.isInstanceOf[ExternalResource]


  private def resourceIRI(e: Resource) =
    e match {
      case StackSetResource(id,_,_,ss,t,_)
      => ModelIRI.resourceInstanceIRI(ss.name,t.name,id)
      case ExternalResource(n,i)
      => ModelIRI.externalEntityIRI(i.name,n)
    }


  private def actionIRI(a: String) =
    ModelIRI.actionIRI(
      a.split(":").head,
      a.split(":").last)


  private def individual(e: Resource) =
    e match {
    case r: StackSetResource
    => df.getOWLNamedIndividual(resourceIRI(r))
    case eR: ExternalResource
    => val extRes = df.getOWLNamedIndividual(resourceIRI(eR))
      permissionsModel
        .ontology
        .add(
          df.getOWLClassAssertionAxiom(
            df.getOWLClass(ModelIRI
              .awsConceptIRI(AwsOntology.ExternalResource)),
            extRes
      ))
      extRes
    }


  private def property(a: String) =
    df.getOWLObjectProperty(actionIRI(a))


  private def conceptFromPrincipal(e: Principal) =
    e match {
      case Public
      => owlPublic()
      case ServicePrincipal(sp)
      => owlClassExpressionFromServicePrincipal(sp)
      case AccountPrincipal(accId)
      => owlClassExpressionFromAccountPrincipal(accId)
      case FederatedAccountPrincipal(fed)
      => owlClassExpressionFromFederatedAccount(fed)
      case CanonicalUserPrincipal(uid)
      => owlClassExpressionFromCanonicalUser(uid)
      case ssR:StackSetResource
      => owlNominalFromResource(ssR)
      case eR:ExternalResource
      => owlNominalFromExternalResource(eR)
    }


    private def owlClassExpressionFromServicePrincipal(sp: String) =
      nominalOf(
        df.getOWLNamedIndividual(ModelIRI
          .awsServicePrincipalIRI(
          sp)))


    private def owlPublic() =
      df.getOWLClass(ModelIRI.awsPublicIRI)


    private def owlClassExpressionFromAccountPrincipal(accId: String) =
    {
      val accountIndividual   =
        df.getOWLNamedIndividual(ModelIRI
          .awsAccountIRI(accId))

      permissionsModel
        .ontology
        .add(df
          .getOWLClassAssertionAxiom(
            df.getOWLClass( ModelIRI
              .awsConceptIRI(AwsOntology.Account)),
            accountIndividual
          ))

      val isOwnedBy           =
        df.getOWLObjectProperty(ModelIRI.
          awsPropertyIRI(AwsOntology.IsOwnedByAccount))

      val hasAccessToAccount  =
        df.getOWLObjectProperty(ModelIRI.
          awsPropertyIRI(AwsOntology.AccessAccount))

      val role =
        df.getOWLClass( ModelIRI.
          resourceTypeIRI("iamrole","role"))

      val user =
        df.getOWLClass( ModelIRI
          .resourceTypeIRI("iamuser","user"))

      val ownedByAccount      =
        df.getOWLObjectSomeValuesFrom(
          isOwnedBy,
          nominalOf(accountIndividual))

      val canAccessAccount    =
        df.getOWLObjectSomeValuesFrom(
          hasAccessToAccount,
          nominalOf(accountIndividual))

      df.getOWLObjectIntersectionOf(
        df.getOWLObjectUnionOf(role,user),
        ownedByAccount)

//      df.getOWLObjectUnionOf(
//        ownedByAccount,
//        canAccessAccount,
//        nominalOf(accountIndividual))
    }


    private def owlClassExpressionFromFederatedAccount(fed: String) =
    {
      val federation  =
        df.getOWLNamedIndividual(ModelIRI
          .awsFederatedAccountIRI(fed))

      permissionsModel
        .ontology
        .add(df.getOWLClassAssertionAxiom(
            df.getOWLClass(ModelIRI.awsConceptIRI(AwsOntology.Federation) ),
            federation
      ))

      val hasAccountWithFederation =
        df.getOWLObjectProperty(ModelIRI
          .awsPropertyIRI(AwsOntology.HasAccountWithFederation))

      df.getOWLObjectSomeValuesFrom(
        hasAccountWithFederation,
        nominalOf(federation))
    }


    private def owlClassExpressionFromCanonicalUser(uid: String) =
      nominalOf(
        df.getOWLNamedIndividual(ModelIRI
          .awsCanonicalUserIRI(uid)))


    private def owlNominalFromResource(ssR: StackSetResource) =
      nominalOf(df
        .getOWLNamedIndividual(
          resourceIRI(ssR)))


    private def owlNominalFromExternalResource(eR: ExternalResource) =
    {
      val externalResource =
        df.getOWLNamedIndividual(ModelIRI
          .externalEntityIRI(infrastructure.name,
            eR.name))

      permissionsModel
        .ontology
        .add(df.getOWLClassAssertionAxiom(
          df.getOWLClass( ModelIRI
            .awsConceptIRI(AwsOntology.ExternalResource) ),
          externalResource))

      nominalOf(externalResource)
    }


}

