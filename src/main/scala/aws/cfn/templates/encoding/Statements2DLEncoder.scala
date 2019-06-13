package aws.cfn.templates.encoding

import aws.cfn.dlmodel.DLModelIRI
import aws.cfn.dlmodel.template.PermissionsModel
import aws.cfn.maps.ActionsMap
import aws.cfn.templates.formalization._
import org.semanticweb.owlapi.model._

import scala.jdk.CollectionConverters._
import scala.language.postfixOps

object Statements2DLEncoder {

  def encode(name: String, statements: Set[Statement], infrastructure: Infrastructure): PermissionsModel = {
    new Statements2DLEncoder(name, statements, infrastructure).encode()
  }

}




private class Statements2DLEncoder(val name: String,
                                   val statements : Set[Statement],
                                   val infrastructure: Infrastructure){

  val permissionsModel : PermissionsModel = new PermissionsModel(name)
  val df : OWLDataFactory = permissionsModel.df

  def encode (): PermissionsModel = {
    permissionsModel.ontology.add(axiomsFromStatements().asJava)
    permissionsModel
  }

  private def axiomsFromStatements() : Set[OWLSubClassOfAxiom] = {
    resourceActionPairs flatMap getAclAxiom
  }


  private def getAclAxiom(ra: (Resource,String)) : Set[OWLSubClassOfAxiom] = {

    val r = individual(ra._1)
    val a = property(ra._2)

    //Set(subclassOf( nominalOf(r) , forAll(inverseOf(a),filler(ra._1,ra._2)) )) ++
    Set(subclassOf( filler(ra._1,ra._2) , performActionOnResource(a,r) ))

  }

  private def performActionOnResource(a:OWLObjectProperty, r:OWLNamedIndividual) : OWLObjectSomeValuesFrom = {
    df.getOWLObjectSomeValuesFrom( a , nominalOf(r) )
  }

  private def subclassOf(lhs: OWLClassExpression, rhs:OWLClassExpression) : OWLSubClassOfAxiom = {
    df.getOWLSubClassOfAxiom(lhs,rhs)
  }

  private def nominalOf(r:OWLNamedIndividual):OWLObjectOneOf = {
    df.getOWLObjectOneOf(r)
  }

  private def forAll(inverseAction: OWLObjectInverseOf,filler: OWLClassExpression) : OWLObjectAllValuesFrom = {
    df.getOWLObjectAllValuesFrom(inverseAction,filler)
  }

  private def inverseOf(act: OWLObjectProperty):OWLObjectInverseOf = {
    df.getOWLObjectInverseOf(act)
  }

  private def filler(r:Principal, a:String) : OWLClassExpression = {

    df.getOWLObjectIntersectionOf(
      overApproximatedAllowSet(r,a),
      not(underApproximatedDenySet(r,a)))
  }

  private def overApproximatedAllowSet(r:Principal,a:String) : OWLClassExpression = {

    def trustedPrincipals(princ:Set[Principal],r:Principal):Set[Principal] =
      princ filter ( p =>
          assumeRoleStatements exists (
              s =>  ( s.principals._1 && ( (s.principals._2 contains r) || (s.principals._2 contains Public))) ||
                    (!s.principals._1  && (!(s.principals._2 contains r) && !(s.principals._2 contains Public)))
                && (s.resources._2.toSet contains p.asInstanceOf[Resource])
        ))

    def checkIfAttemptingToAssumeRole(s: Statement) : Set[Principal] = {
      if (s.actions._2 contains "sts:AssumeRole") {
        trustedPrincipals( s.principals._2 ,r)
      }
      else
        s.principals._2
    }

    (allowStatements & statementsWithResource(r) & statementsWithAction(a) /*& notAssumeRoleStatements*/).
      foldLeft[OWLClassExpression](df.getOWLNothing)(
        (filler,s) => df.getOWLObjectUnionOf(filler,actualPrincipals(true,checkIfAttemptingToAssumeRole(s))))
  }


  private def underApproximatedDenySet(r:Entity,a: String) : OWLClassExpression = {
    (denyStatements & statementsWithResource(r) & statementsWithAction(a) /*& notAssumeRoleStatements*/).
      foldLeft[OWLClassExpression](df.getOWLNothing)( (filler,s) =>
      df.getOWLObjectUnionOf(filler, if (s.hasCondition) df.getOWLNothing else actualPrincipals(false,s.principals._2))
    )
  }

  private def not(ce: OWLClassExpression) =
    df.getOWLObjectComplementOf(ce)

  private def actualPrincipals(isAllow: Boolean, principals:Set[Principal]) : OWLClassExpression = {

    val conceptPrincipals =
      (principals map conceptFromPrincipal)
      .foldLeft[OWLClassExpression](df.getOWLNothing)((filler,c) => df.getOWLObjectUnionOf(filler,c) )

    if (isAllow) conceptPrincipals else not(conceptPrincipals)

  }


  private def resourceActionPairs() : Set[(Resource,String)] = {

    def actionCanBePerformedOverResource(a:String,r:Entity):Boolean =
      ActionsMap.lookUpActionPrefix(r.asInstanceOf[StackSetResource].serviceType) contains a

    def actionIsInStatementsWithEntity(a:String,r:Entity):Boolean =
      statementsWithResource(r) & statementsWithAction(a) nonEmpty

    for { s <- statements; r <- actualResources(s) ; a <- actualActions(s)
          if  (isResource(r) && actionCanBePerformedOverResource(a,r )) ||
              ( isExternalEntity(r) && actionIsInStatementsWithEntity(a,r))
    } yield (r,a)

  }

  private def actualResources(statement: Statement) : Set[Resource] = {

    def complementOf(resources : Set[Resource]) : Set[Resource] =
      (for ( s <- statements; r <- s.resources._2) yield r) &~ resources

    if ( statement.resources._1 )
      statement.resources._2.toSet
    else
      complementOf(statement.resources._2.toSet)
  }


  private def actualActions(statement: Statement) : Set[String] = {

    def complementOf(actions : Set[String]) : Set[String] =
      (for ( s <- statements; a <- s.actions._2) yield a) &~ actions

    if ( statement.actions._1 )
      statement.actions._2.toSet
    else
      complementOf(statement.actions._2.toSet)
  }




  private def allowStatements = statements filter ( s => s.isInstanceOf[AllowStatement])
  private def denyStatements = statements filter ( s => s.isInstanceOf[DenyStatement])
  private def notAssumeRoleStatements = statements filter ( s => !s.isAssumeRoleStatement )
  private def assumeRoleStatements = statements filter ( s => s.isAssumeRoleStatement)
  private def statementsWithResource(r:Entity):Set[Statement] =
    statements filter ( s => s.resources._2 contains r)
  private def statementsWithAction(a:String):Set[Statement] =
    statements filter ( s => s.actions._2 contains a)
  private def isResource(e:Entity):Boolean          = e.isInstanceOf[StackSetResource]
  private def isExternalEntity(e:Entity):Boolean    = e.isInstanceOf[ExternalResource]

  private def resourceIRI(e:Resource) = e match {
    case StackSetResource(n,_,_,ss,_)   => DLModelIRI.resourceInstanceIRI(ss.name,n)
    case ExternalResource(n,i)    => DLModelIRI.externalEntityIRI(i.name,n)
  }

  private def actionIRI(a:String) = DLModelIRI.actionIRI(a.split(":").head,a.split(":").last)
  private def individual(e:Resource) = e match {
    case r:StackSetResource => df.getOWLNamedIndividual(resourceIRI(r))
    case eR:ExternalResource => {
      val extRes = df.getOWLNamedIndividual(resourceIRI(eR))
      permissionsModel.ontology.add(df.getOWLClassAssertionAxiom(
        df.getOWLClass( DLModelIRI.awsConceptIRI("ExternalResource") ),
        extRes
      ))
      extRes
    }
  }
  private def property(a:String) = df.getOWLObjectProperty(actionIRI(a))



  private def conceptFromPrincipal(e:Principal) : OWLClassExpression =
    e match {
      case Public               => df.getOWLClass(DLModelIRI.awsPublicIRI)
      case ServicePrincipal(sp) => nominalOf( df.getOWLNamedIndividual(DLModelIRI.awsServicePrincipalIRI(sp)) )
      case AccountPrincipal(accId)  => {
        val accountIndividual   = df.getOWLNamedIndividual(DLModelIRI.awsAccountIRI(accId))
        permissionsModel.ontology.add(df.getOWLClassAssertionAxiom(
          df.getOWLClass( DLModelIRI.awsConceptIRI("AwsAccount") ),
          accountIndividual
        ))
        val isOwnedBy           = df.getOWLObjectProperty(DLModelIRI.awsPropertyIRI("isOwnedByAccount"))
        val hasAccessToAccount  = df.getOWLObjectProperty(DLModelIRI.awsPropertyIRI("hasAccessToAccount"))
        val ownedByAccount      = df.getOWLObjectSomeValuesFrom(isOwnedBy,nominalOf(accountIndividual))
        val canAccessAccount    = df.getOWLObjectSomeValuesFrom(hasAccessToAccount,nominalOf(accountIndividual))
        df.getOWLObjectUnionOf(ownedByAccount,canAccessAccount,nominalOf(accountIndividual))
      }
      case FederatedAccountPrincipal(fed) => {
        val federation            = df.getOWLNamedIndividual(DLModelIRI.awsFederatedAccountIRI(fed))
        permissionsModel.ontology.add(df.getOWLClassAssertionAxiom(
          df.getOWLClass( DLModelIRI.awsConceptIRI("FederatedAccount") ),
          federation
        ))
        val hasAccessToFedAccount = df.getOWLObjectProperty(DLModelIRI.awsPropertyIRI("hasAccessToFederatedAccount"))
        df.getOWLObjectSomeValuesFrom(hasAccessToFedAccount,nominalOf(federation))
      }
      case CanonicalUserPrincipal(uid) => {
        nominalOf( df.getOWLNamedIndividual(DLModelIRI.awsCanonicalUserIRI(uid)) )
      }
      case ssR:StackSetResource => nominalOf(df.getOWLNamedIndividual(resourceIRI(ssR)))
      case eR:ExternalResource  => {
        val externalResource = df.getOWLNamedIndividual(DLModelIRI.externalEntityIRI(infrastructure.name,eR.name))
        permissionsModel.ontology.add(df.getOWLClassAssertionAxiom(
          df.getOWLClass( DLModelIRI.awsConceptIRI("ExternalResource") ),
          externalResource
        ))
        nominalOf(externalResource)
      }
    }



}