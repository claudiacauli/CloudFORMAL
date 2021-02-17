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

import java.io.File

import com.cloud.formal.dataflow.{LabelsUtils => LU}
import com.cloud.formal.dataflow.{OntologyUtils => OU}
import com.cloud.formal.dataflow.{IRIUtils => IU}
import com.cloud.formal.dataflow.LabelsUtils.{makeLabelRecords, makeNameTerminal}
import com.cloud.formal.model.ModelFileSuffix
import org.semanticweb.owlapi.model.{OWLDataFactory, OWLNamedIndividual, OWLOntology, OWLOntologyManager}
import com.cloud.formal.{Extension, Ontology, Renaming}

import scala.jdk.StreamConverters._
import Console.{BOLD, RESET}



object InfrastructureGraph extends Graph {

  //val CURRENTDIR = "src/main/scala/com/cloud/formal/dataflow/"
  //val infrModelPath = "src/main/scala/com/cloud/formal/dataflow/out/16_CaseStudy/16_CaseStudy_InfrastructureModel.owl"

  val GRAPHTAG = "graph"

  val dotFileName: String = GRAPHTAG + Extension.DOT
  val graphFileName: String = GRAPHTAG + Extension.PNG
  var o: OWLOntology = _
  var df: OWLDataFactory = _
  var m: OWLOntologyManager = _

  def run(path: String): Unit =
  {
    var outPath = path
    var inPath = path

    if (path.endsWith(ModelFileSuffix.Infrastructure))
      outPath = path.split(ModelFileSuffix.Infrastructure)(0)
    else
      inPath = new File(path).listFiles().find(_.getName.endsWith(ModelFileSuffix.Infrastructure)).get.getAbsolutePath

    if (!outPath.endsWith("/"))
      outPath = outPath + "/"

    println(s"\n$BOLD Building Graph of " + inPath.split("/").last + s"$RESET")


    initializeGraphGenerator(inPath)
    val reasEngine = OU.startReasoner(o,df,m)
    OU.addInferredAxiomsToMainOntologyManager(o,m,reasEngine)
    printGraphToDotFile(dotFileName,buildGraphDotSpecification, outPath)
    runGraphvizDotToImage(dotFileName,graphFileName, outPath)
    println(s"$BOLD graph.png written to path: " + new File(outPath).getAbsolutePath + s"$RESET\n")

  }



















  // BUILDING DOT SPECIFICATION, PRINTING AND WRITING GRAPH TO FILE
  private
  def buildGraphDotSpecification: String =
  {
      (getSubgraphsDefinitions ++
      getNodeDefinitionFromSubProperty ++
      getNodeDefinitionFromResource ++
      getEdgesDefinitionForObjectProperties)
        .foldLeft("digraph G {")( (a,n) => a+n )+"\n}"
  }







  //
  //
  // "GET-GRAPH-DEFINITION-FROM" Methods (Resources, Terminals, Subproperties, Obj Props)

  private
  def getNodeDefinitionFromResource: Set[String] =
    OU.getAllResourceNodes(m) map (n => {

      val resType = OU.getAllClassAssertions(m).filter(x => x.getIndividual == n)
        .filter(ax => ax.getClassExpression.isClassExpressionLiteral)
        .map ( ax => ax.getClassExpression.asOWLClass().getIRI.toString.split("/").last )
        .find( resType => Set("s3bucket#bucket","snstopic#topic",
          "sqsqueue#queue","apigatewayrestapi#restapi",
          "lambdafunction#function","iamrole#role") contains resType )

      val label =
        if (OU.getAllDataRecordOnlyNodes(m) contains n)
          LU.makeLabelRecords(n,m)
        else if (OU.getAllSomeDataRecordNodes(m) contains n)
          LU.makeLabelRecordsWithName(n,LU.makeLabelResourceNode(n),m)
        else
          LU.makeLabelResourceNode(n)

      val shape = resType match {

        case Some(s) =>
          val label1 =
            if (label.startsWith("<<") && label.endsWith(">>"))
              label.substring(1,label.length-1)
            else label
          "shape=box fixedsize=true width=2 height=1.5 color=white "+
            "label=<<table border=\"0\" cellspacing=\"0\" cellpadding=\"0\">" +
            "<tr><td border=\"0\" fixedsize=\"true\" width=\"75\" height=\"75\" ><img src=\"src/main/resources/aws-icons/" + s.split(Ontology.Pound).head + ".png\"/></td></tr>" +
            "<tr><td border=\"0\" >"+ label1 +"</td></tr>" +
            "</table>> labelloc=b "

        case None =>
          val label1 =
            if (label.startsWith("<<") && label.endsWith(">>"))
              label
            else "\""+label+"\""
          //"shape=circle fixedsize=true width=1.5 height=1.5 style=filled color=gold fillcolor=gold "+
          "label=" +
            ""+ label1



      }

      "\n  " + LU.makeNameResourceNode(n) +
        " ["+shape+" ]"
    })



  private
  def getSubgraphDefinitionFromAccount(n: OWLNamedIndividual) =
    "\n  subgraph cluster_" + IU.getSimpleName(n) + " {" +
      "shape=box style=bold color=grey label = \"Account: "+IU.getSimpleName(n)+"\"" +
      getAllNodesOwnedByAccount(n).foldLeft("")((a,x) => a + " " + LU.makeName(x,m) ) +
      "}"

  private
  def getSubgraphsDefinitions : List[String]= {
    OU.getAllAccounts(m,df) map getSubgraphDefinitionFromAccount
  }

  // SubpropertyNodes


//  private
//  def getNodeDefinitionFromTerminal: Set[String] =
//    OU.getAllTerminalAsString(m) map (s => "\n  "+
//      makeNameTerminal(s)
//      +" [shape=box style=dashed color=lightgray label=\""+
//      LU.makeLabelTerminal(s) +"\"]")


  private
  def getNodeDefinitionFromSubProperty: List[String] = {
    OU.getAllSubPropertyNodes(m) map (n =>{
      if (OU.getAllDataRecordOnlyNodes(m) contains n)
        "\n  " + LU.makeNameSubProperty(n) +
          " [shape=plain fixedsize=true width=0.3 height=0.3 color=lightgray label="+ LU.makeLabelRecords(n,m) +"]"
      else if (OU.getAllSomeDataRecordNodes(m) contains n)
        "\n  " + LU.makeNameSubProperty(n) +
          " [shape=circle fixedsize=true width=0.3 height=0.3 style=filled color=lightgray fillcolor=lightgray label="+makeLabelRecords(n,m)+"]"
      else "\n  " + LU.makeNameSubProperty(n) +
        " [shape=circle fixedsize=true width=0.3 height=0.3 style=filled color=lightgray fillcolor=lightgray label=\"\"]"
    })
  }



  private
  def getEdgesDefinitionForObjectProperties: Set[String] =
    OU.getAllObjectPropertyAssertions(m)
      //.filter(dpa => !isDataRecordNode(dpa.getSubject.asOWLNamedIndividual()))
      .filterNot( dpa =>
      IU.isAwsOntologyObject(dpa.getObject.asOWLNamedIndividual()) || IU.isAwsOntologyObject(dpa.getSubject.asOWLNamedIndividual())
        || IU.isAwsOntologyObject(dpa.getProperty.asOWLObjectProperty()))
      .map (dpa => {
        "\n  " +
          (if (IU.getSimpleName(dpa.getSubject.asOWLNamedIndividual()).startsWith(LU.SubpropertyString))
            LU.makeNameSubProperty(dpa.getSubject.asOWLNamedIndividual())
          else
            LU.makeNameResourceNode(dpa.getSubject.asOWLNamedIndividual())) +
          " -> " +
          (if (IU.getSimpleName(dpa.getObject.asOWLNamedIndividual()).startsWith(LU.SubpropertyString))
            LU.makeNameSubProperty(dpa.getObject.asOWLNamedIndividual())
          else
            LU.makeNameResourceNode(dpa.getObject.asOWLNamedIndividual())) +
          " [shape=curve label=\""+ IU.getSimpleName(dpa.getProperty.asOWLObjectProperty()).split(Renaming.Delimiter).last +"\"]"
      })


  private
  def getAllNodesOwnedByAccount(acc: OWLNamedIndividual) =
    m.ontologies().toScala(List).flatMap(o => {
      OU.getAllObjPropAssertionsIndividual(o,acc)
        .filter (opax => IU.getSimpleName(opax.getProperty.asOWLObjectProperty()).endsWith("owns"))
        .flatMap(opax => allSubPropertyNodesOf(opax.getObject.asOWLNamedIndividual()))
    })


  private
  def allSubPropertyNodesOf (n: OWLNamedIndividual) : Set[OWLNamedIndividual] =
    m.ontologies().toScala(List)
      .flatMap ( o => OU.getAllObjPropAssertionsIndividual(o,n) )
      .filter ( opax => OU.getAllSubPropertyNodes(m) contains opax.getObject)
      .flatMap ( opax =>
        Set(opax.getObject.asOWLNamedIndividual()) union allSubPropertyNodesOf(opax.getObject.asOWLNamedIndividual()) ).toSet
      .union(Set(n))




}
