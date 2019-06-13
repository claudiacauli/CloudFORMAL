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


  private def getAclAxiom(ra: (Entity,String)) : Set[OWLSubClassOfAxiom] = {

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

  private def filler(r:Entity, a:String) : OWLClassExpression = {

    df.getOWLObjectIntersectionOf(
      overApproximatedAllowSet(r,a),
      not(underApproximatedDenySet(r,a)))
  }

  private def overApproximatedAllowSet(r:Entity,a:String) : OWLClassExpression = {

    def trustedPrincipals(princ:Set[Entity],r:Entity):Set[Entity] =
      princ filter ( p =>
          assumeRoleStatements exists (
              s =>  ( s.principals._1 && ( (s.principals._2 contains r) || (s.principals._2 contains Public))) ||
                    (!s.principals._1  && (!(s.principals._2 contains r) && !(s.principals._2 contains Public)))
                && (s.resources._2.toSet contains p)
        ))

    def checkIfAttemptingToAssumeRole(s: Statement):Set[Entity] = {
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

  private def actualPrincipals(isAllow: Boolean, principals:Set[Entity]) : OWLClassExpression = {

    val conceptPrincipals =
      (principals map conceptOf)
      .foldLeft[OWLClassExpression](df.getOWLNothing)((filler,c) => df.getOWLObjectUnionOf(filler,c) )

    if (isAllow) conceptPrincipals else not(conceptPrincipals)

  }


  private def resourceActionPairs() = {

    def actionCanBePerformedOverResource(a:String,r:Entity):Boolean =
      ActionsMap.lookUpActionPrefix(r.asInstanceOf[Resource].serviceType) contains a

    def actionIsInStatementsWithEntity(a:String,r:Entity):Boolean =
      statementsWithResource(r) & statementsWithAction(a) nonEmpty

    for { s <- statements; r <- actualResources(s) ; a <- actualActions(s)
          if  (isResource(r) && actionCanBePerformedOverResource(a,r )) ||
              ( isExternalEntity(r) && actionIsInStatementsWithEntity(a,r))
    } yield (r,a)

  }

  private def actualResources(statement: Statement) : Set[Entity] = {

    def complementOf(resources : Set[Entity]) : Set[Entity] =
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
  private def isResource(e:Entity):Boolean          = e.isInstanceOf[Resource]
  private def isExternalEntity(e:Entity):Boolean    = e.isInstanceOf[ExternalEntity]
  private def entityIRI(e:Entity) = e match {
    case Resource(n,_,_,ss,_)   => DLModelIRI.resourceInstanceIRI(ss.name,n)
    case ExternalEntity(n,i)    => DLModelIRI.externalEntityIRI(i.name,n)
    case ServicePrincipal(n,i)  => DLModelIRI.servicePrincipalIRI(i.name,n)
  }
  private def actionIRI(a:String) = DLModelIRI.actionIRI(a.split(":").head,a.split(":").last)
  private def individual(e:Entity) = df.getOWLNamedIndividual(entityIRI(e))
  private def property(a:String) = df.getOWLObjectProperty(actionIRI(a))
  private def conceptOf(e:Entity) : OWLClassExpression = e match {
    case Public => df.getOWLClass(DLModelIRI.publicEntityIRI)
    case x => nominalOf(df.getOWLNamedIndividual(entityIRI(x)))
  }
}