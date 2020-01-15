package com.cloud.formal.reasoning

import com.cloud.formal.reasoning.QueryOutcome.QueryOutcome
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.reasoner._
import uk.ac.manchester.cs.jfact.JFactFactory
import uk.ac.manchester.cs.jfact.kernel.options.JFactReasonerConfiguration
import org.semanticweb.HermiT.ReasonerFactory


object Reasoner {

  def create(o: OWLOntology, df: OWLDataFactory, m: OWLOntologyManager) =
    new Reasoner(o,df,m)

}

class Reasoner(ontology: OWLOntology, df: OWLDataFactory, manager: OWLOntologyManager)
{

  val reasoner = (new JFactFactory)
    .createNonBufferingReasoner(
      ontology,
      new JFactReasonerConfiguration())


  def classify(): Unit =
  {
    reasoner.flush()
    val t = System.currentTimeMillis()
    print("\n  Precomputing inferences... ")

    reasoner.precomputeInferences(InferenceType.CLASS_HIERARCHY)
    reasoner.precomputeInferences(InferenceType.DISJOINT_CLASSES)
    reasoner.precomputeInferences(InferenceType.DATA_PROPERTY_HIERARCHY)
    reasoner.precomputeInferences(InferenceType.OBJECT_PROPERTY_HIERARCHY)
    reasoner.precomputeInferences(InferenceType.SAME_INDIVIDUAL)
    reasoner.precomputeInferences(InferenceType.DIFFERENT_INDIVIDUALS)
    reasoner.precomputeInferences(InferenceType.CLASS_ASSERTIONS)
    reasoner.precomputeInferences(InferenceType.OBJECT_PROPERTY_ASSERTIONS)
    reasoner.precomputeInferences(InferenceType.DATA_PROPERTY_ASSERTIONS)

    //assert(reasoner.isConsistent)

    val unsatisfiable = reasoner
      .getUnsatisfiableClasses.getEntitiesMinusBottom

    if (!unsatisfiable.isEmpty) {
      println(s" (took ${System.currentTimeMillis()-t} ms).\n"+
        "The following classes are unsatisfiable: ")
      unsatisfiable.forEach(c => println(c.getIRI.getFragment))
    }
    else
      println(s" (took ${System.currentTimeMillis()-t} ms).\n"+
        "\t(The model is consistent and there are no unsatisfiable classes)")
  }


  def runQuery(expr: OWLClassExpression):
  (QueryOutcome,Option[NodeSet[OWLNamedIndividual]]) = {
    if (reasoner.isSatisfiable(expr))
      hasInstances(expr)
    else (QueryOutcome.UNSAT, None)
  }



  private def hasInstances(expr: OWLClassExpression):
      (QueryOutcome,Option[NodeSet[OWLNamedIndividual]]) = {
     val set = this.getInstances(expr)
        if (set.isEmpty)
          ( QueryOutcome.SAT0, None )
        else ( QueryOutcome.SAT1, Some(set) )
  }



  def getInstances(expr: OWLClassExpression) = {
    reasoner.getInstances(expr, InferenceDepth.ALL)
  }



  // TODO Double check if you need these two

//  private def isAxiomEntailed(axiom: OWLAxiom): QueryResult =
//    if (reasoner.isEntailed(axiom: OWLAxiom))
//      QueryResult.ENTAILED
//    else QueryResult.NOT_ENTAILED
//
//
//  def isClassAssertionEntailed(i: OWLNamedIndividual, c: OWLClassExpression): QueryResult =
//    isAxiomEntailed(df.getOWLClassAssertionAxiom(c,i))


}



