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

import java.io.{File, PrintWriter}

import com.cloud.formal.reasoning.QueryOutcome.QueryOutcome
import org.semanticweb.owlapi.model.{OWLClassExpression, OWLDataFactory, OWLNamedIndividual, OWLOntology, OWLOntologyManager}
import org.semanticweb.owlapi.reasoner.{InconsistentOntologyException, NodeSet}

class PropertiesChecker (name: String, o: OWLOntology, df: OWLDataFactory, m: OWLOntologyManager, dir: String = ".") {

  private[formal] val propsVec  = PropertiesGenerator.init()
  private[formal] val r         = Reasoner.create(o, df, m)
  private[formal] val pE        = new PropertyEval(r,o,df,m)

  def classify(printEnabled: Boolean = true): Unit =
    r.classify(printEnabled)(r.computeAllInferences)

  def run(printEnabled: Boolean)(runQueryFun: ((=> OWLClassExpression) => Boolean) => (=>OWLClassExpression) => (QueryOutcome,Option[NodeSet[OWLNamedIndividual]]) )
         (satCheckFun: Boolean => (=> OWLClassExpression) => Boolean): Unit =
  {
    val pw = new PrintWriter(new File(dir+"/"+name+"Report.csv"))
    var reportString = ""
    try {
      classify()
      propsVec.sortBy(_._1).foreach( p => {
        val outcome = pE.evalAndPrint(p._2, printEnabled)(runQueryFun)(satCheckFun)
        outcome match {
          case None      => reportString += (p._2.id+","+"N/A\n")
          case Some(oc)  =>
            reportString += (p._2.getPassOrFilePrint(oc)+","+p._2.id + "," + p._2.getOutcomePrint(oc)+"\n")
        }
      })
      pw.write(reportString)
    } catch {
      case e: InconsistentOntologyException
        => println("  INCONSISTENT ONTOLOGY FOUND. SKIPPING. ")
        Vector()
    }
    pw.close()
  }

  def runEach (p: Property, printEnabled: Boolean)
  (runQueryFun: ((=> OWLClassExpression) => Boolean) => (=>OWLClassExpression) => (QueryOutcome,Option[NodeSet[OWLNamedIndividual]]) )
  (satCheckFun: Boolean => (=> OWLClassExpression) => Boolean): Unit =
    pE.evalAndPrint(p, printEnabled)(runQueryFun)(satCheckFun)

}
