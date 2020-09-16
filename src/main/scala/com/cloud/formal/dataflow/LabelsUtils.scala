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

import com.cloud.formal.Renaming
import com.cloud.formal.dataflow.{OntologyUtils => OU}
import org.semanticweb.owlapi.model.{OWLNamedIndividual, OWLOntologyManager}

import scala.jdk.StreamConverters._

object LabelsUtils {

  val SubpropertyString = "subproperty"

  def makeNameSubProperty(n: OWLNamedIndividual): String =
    n.getIRI.toString.split("subproperty_")(1).replaceAll("[^0-9]","")


  def makeNameTerminal(s: String): String =
    makeLabelTerminal(s).replaceAll("[^a-zA-Z0-9_]", "")

  private[dataflow]
  def makeLabelTerminal(s: String) =
  {
    val s1 = s.replaceAll("\"","")
    if (s1.length > 20)
      s1.substring(0,20)+"..."
    else s1
  }



  private[dataflow]
  def makeName(i: OWLNamedIndividual, m: OWLOntologyManager) =
    if (OU.getAllResourceNodes(m) contains i)
      makeNameResourceNode(i)
    else makeNameSubProperty(i)



  private[dataflow]
  def makeNameResourceNode(i: OWLNamedIndividual) =
    makeLabelResourceNode(i).replaceAll("[^a-zA-Z0-9]","")


  private[dataflow]
  def makeLabelResourceNode(i: OWLNamedIndividual) =
  {
    val s = i.getIRI.toString.split(Renaming.Delimiter).last
    if (s.length > 20)
      s.substring(0,20)+"..."
    else s
  }

  private[dataflow]
  def makeLabelRecordsWithName(n: OWLNamedIndividual, name: String, m: OWLOntologyManager): String =
    m.ontologies().toScala(List)
      .flatMap( o => OU.getAllDataPropAssertionsIndividual(o,n))
      .foldLeft("<<b>" + name + "</b>")((a,dpax) => a +
        ""  // TODO Temporarily disabling the print of all the single attribute-value pairs
        //        getSimpleName(dpax.getProperty.asOWLDataProperty()).split("_").last +
        //        " : <i>" + makeLabelTerminal(dpax.getObject.toString.split("\\^\\^")(0)) + "</i><br/>"
      ) + ">"


  def makeLabelRecords(n: OWLNamedIndividual, m: OWLOntologyManager): String =
    m.ontologies().toScala(List)
      .flatMap( o => OU.getAllDataPropAssertionsIndividual(o,n))
      .foldLeft("<")((a,dpax) => a +
        ""  // TODO Temporarily disabling the print of all the single attribute-value pairs
        //      getSimpleName(dpax.getProperty.asOWLDataProperty()).split(Renaming.Delimiter).last +
        //      " : <i>" + makeLabelTerminal(dpax.getObject.toString.split("\\^\\^")(0)) + "</i><br/>"
      ) + ">"




}
