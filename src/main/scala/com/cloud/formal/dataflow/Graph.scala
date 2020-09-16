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

package com.cloud.formal.dataflow

import java.io.PrintWriter

import scala.sys.process._
import org.semanticweb.owlapi.model.{OWLDataFactory, OWLOntology, OWLOntologyManager}

trait Graph {

  protected val dotFileName: String
  protected val graphFileName: String
  protected var o: OWLOntology
  protected var df: OWLDataFactory
  protected var m: OWLOntologyManager


  protected
  def initializeGraphGenerator(infrastructureModelFilePath: String): Unit =
  {
    val p = OntologyUtils.loadInfrastructureModel(infrastructureModelFilePath)
    o = p._1
    df = p._2
    m = p._3
  }


  protected
  def printGraphToDotFile(graphFileName: String, graphString: String): PrintWriter =
  {
    print(graphString)
    new PrintWriter(graphFileName){write(graphString); close()}
  }


  protected
  def runGraphvizDotToImage(dotFileName: String, graphFileName: String): Int =
    ("dot -Tpng " + dotFileName + " -o " + graphFileName).!


}
