/*
 *    Copyright 2019 Claudia Cauli
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */

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

  private val reasoner: OWLReasoner = jFactReasoner()



  def classify(printEnabled: Boolean = true)(classificationFunction: => OWLReasoner => Unit): Unit =
  {

    val axiomsCountPrint = "[Logical Axioms Count: "+ontology.getLogicalAxiomCount(Imports.INCLUDED)+"]"
    if (printEnabled) println(f"\t\t$axiomsCountPrint%-5s")

    classificationFunction(reasoner)

    val unsatisfiable = reasoner
      .getUnsatisfiableClasses.getEntitiesMinusBottom

    if (!unsatisfiable.isEmpty) {
      println("The model is inconsistent. The following classes are unsatisfiable: ")
      unsatisfiable.forEach(c => println(c.getIRI.getFragment))
    }
    else if (printEnabled) println("The model is consistent and there are no unsatisfiable classes\n")
  }



  private[formal] def jFactReasoner(): OWLReasoner = {
    (new JFactFactory)
      .createNonBufferingReasoner(
        ontology,
        new JFactReasonerConfiguration())
  }



  private[formal] def computeAllInferences(reasoner: OWLReasoner): Unit = {
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




  def runQuery(satFunction: (=> OWLClassExpression) => Boolean)(expr: => OWLClassExpression):
  (QueryOutcome,Option[NodeSet[OWLNamedIndividual]]) =
    if (satFunction(expr))
      hasInstances(expr)
    else unsatOutcome


  def unsatOutcome: (QueryOutcome,Option[NodeSet[OWLNamedIndividual]])
  = (QueryOutcome.UNSAT, None)


  def isSat(hasReqResources: Boolean)(expr: => OWLClassExpression): Boolean = {
    val t = System.nanoTime()
    if (hasReqResources)
      reasoner.isSatisfiable(expr)
    else {
      false
    }
  }






  def hasInstances(expr: OWLClassExpression):
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



