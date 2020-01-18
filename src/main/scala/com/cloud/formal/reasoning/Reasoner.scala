package com.cloud.formal.reasoning

import com.cloud.formal.BenchmarkRunner
import com.cloud.formal.reasoning.QueryOutcome.QueryOutcome
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.reasoner._
import uk.ac.manchester.cs.jfact.JFactFactory
import uk.ac.manchester.cs.jfact.kernel.options.JFactReasonerConfiguration



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

    val axiomsCountPrint = "[Logical Axioms Count: "+ontology.getLogicalAxiomCount(Imports.INCLUDED)+"]"
    println(f"\t\t${axiomsCountPrint}%-5s")

    //BenchmarkRunner.timeNwPreFun(20,100)("Classification", createReasoner, computeAllInferences)
    computeAllInferences(reasoner)

    val unsatisfiable = reasoner
      .getUnsatisfiableClasses.getEntitiesMinusBottom

    if (!unsatisfiable.isEmpty) {
      println("The model is inconsistent. The following classes are unsatisfiable: ")
      unsatisfiable.forEach(c => println(c.getIRI.getFragment))
    }
    else println("The model is consistent and there are no unsatisfiable classes")
  }



  private def createReasoner(): OWLReasoner = {
    (new JFactFactory)
      .createNonBufferingReasoner(
        ontology,
        new JFactReasonerConfiguration())
  }


  private def computeAllInferences(reasoner: OWLReasoner) = {
    reasoner.precomputeInferences(InferenceType.CLASS_HIERARCHY)
    reasoner.precomputeInferences(InferenceType.DISJOINT_CLASSES)
    reasoner.precomputeInferences(InferenceType.DATA_PROPERTY_HIERARCHY)
    reasoner.precomputeInferences(InferenceType.OBJECT_PROPERTY_HIERARCHY)
    reasoner.precomputeInferences(InferenceType.SAME_INDIVIDUAL)
    reasoner.precomputeInferences(InferenceType.DIFFERENT_INDIVIDUALS)
    reasoner.precomputeInferences(InferenceType.CLASS_ASSERTIONS)
    reasoner.precomputeInferences(InferenceType.OBJECT_PROPERTY_ASSERTIONS)
    reasoner.precomputeInferences(InferenceType.DATA_PROPERTY_ASSERTIONS)
  }





  def runQuery(expr: OWLClassExpression):
  (QueryOutcome,Option[NodeSet[OWLNamedIndividual]]) = {

    //val isSat = BenchmarkRunner.timeN(10,100)("Satisfiability Query",reasoner.isSatisfiable(expr))

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



