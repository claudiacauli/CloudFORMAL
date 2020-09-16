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

import com.cloud.formal.{FilePath, Ontology, Extension => Ex}

import com.cloud.formal.dataflow.{LabelsUtils => LU}
import com.cloud.formal.dataflow.{OntologyUtils => OU}
import com.cloud.formal.dataflow.{IRIUtils => IU}
import org.semanticweb.owlapi.model.{AddImport, OWLClassAssertionAxiom, OWLDataFactory, OWLDataPropertyAssertionAxiom, OWLNamedIndividual, OWLObjectPropertyAssertionAxiom, OWLOntology, OWLOntologyAlreadyExistsException, OWLOntologyManager}

import scala.jdk.StreamConverters._


object DataflowGraph extends Graph {

  val CURRENTDIR = "src/main/scala/com/cloud/formal/dataflow/"
  val dotFileName = CURRENTDIR + "DataflowDiagram.dot"
  val graphFileName = CURRENTDIR + "DataflowDiagram.png"
  var o: OWLOntology = _
  var df: OWLDataFactory = _
  var m: OWLOntologyManager = _


  def run(): Unit =
  {
    initializeGraphGenerator("src/main/scala/com/cloud/formal/dataflow/out/16_CaseStudy/16_CaseStudy/16_CaseStudy_InfrastructureModel.owl")

    println("Initialized Graph Generator")

    loadAllOntologiesNeededForDFD()
    println("Loaded all needed ontologies")
    val reasEngine = OU.startReasoner(o,df,m)
    println("Started reasoner")
    val t = System.currentTimeMillis()
    OU.addInferredAxiomsToMainOntologyManager(o,m,reasEngine)
    println("Merged axioms (took " + (System.currentTimeMillis()-t) + " ms).\n\n")
    printGraphToDotFile(dotFileName,buildGraphDotSpecification)

    runGraphvizDotToImage(dotFileName,graphFileName)
  }








  private
  def loadAllOntologiesNeededForDFD(): Unit =
  {

    new File(FilePath.ResourceTerms)
    .listFiles().filter(_.getName.endsWith(Ex.Owl))
      .foreach(f => {
        try {
          m.applyChange(
            new AddImport(o,
            df.getOWLImportsDeclaration(
              m.getOntologyDocumentIRI(m.loadOntologyFromOntologyDocument(f)))))
        }
        catch {
          case _:OWLOntologyAlreadyExistsException =>
        }
      })

    val dfdPath = FilePath.ProjectResources + "Dataflow/"
    Set("dfd.owl","dfdapigateway.owl","dfdcloudwatch.owl",
      "dfdcloudtrail.owl","dfddynamodb.owl","dfdlambda.owl",
      "dfds3.owl","dfdsns.owl","dfdsqs.owl").foreach( s => {
      val imported = m.loadOntologyFromOntologyDocument(new File(dfdPath + s))
      m.applyChange(new AddImport(o, df.getOWLImportsDeclaration( m.getOntologyDocumentIRI(imported) )))
    })


  }













  private
  def buildGraphDotSpecification: String =
  {
    (getSubgraphsDefinitions ++
      getNodeDefinitionFromResource ++
      getEdgesDefinitionForDFDProperties)
        .foldLeft("digraph G {")( (a,n) => a+n )+"\n}"
  }


  private
  def getSubgraphsDefinitions =
    OU.getAllAccounts(m,df) map getSubgraphDefinitionFromAccount


  private
  def getSubgraphDefinitionFromAccount(n: OWLNamedIndividual) =
  {
    "\n  subgraph cluster_" + IU.getSimpleName(n) +
      " {" +
      "shape=box style=bold color=grey label = \"Account: "+IU.getSimpleName(n)+"\"" +
      (getAllNodesOwnedByAccount(n).toSet intersect nodesInvolvedInDFDPropAssertions )
        .foldLeft("")((a,x) => a + " " + LU.makeName(x,m) ) +
      "}"
  }





  private
  def getNodeDefinitionFromResource = {

    (OU.getAllResourceNodes(m) intersect nodesInvolvedInDFDPropAssertions)
      .map (n => {
        val resType = discoverResourceType(n)
        val label = getLabelDependingOnRecordType(n)

        val shape = resType match {
          case Some(s) =>
            val label1 =
              if (label.startsWith("<<") && label.endsWith(">>"))
                label.substring(1,label.length-1)
              else label
            val color = "shape=box color=white "
            color+"fixedsize=true width=2 height=1.5 "+
              "label=<<table border=\"0\" cellspacing=\"0\" cellpadding=\"0\">" +
              "<tr><td border=\"0\" fixedsize=\"true\" width=\"75\" height=\"75\" ><img src=\"src/main/resources/aws-icons/" + s.split(Ontology.Pound).head + ".png\"/></td></tr>" +
              "<tr><td border=\"0\" >"+ label1 +"</td></tr>" +
              "</table>> labelloc=b "
          case None =>
            val label1 =
              if (label.startsWith("<<") && label.endsWith(">>"))
                label
              else "\""+label+"\""
            "label=" + ""+ label1
        }

        "\n  " + LU.makeNameResourceNode(n) +
          " ["+shape+" ]"
      })

  }


  private
  def getEdgesDefinitionForDFDProperties =
    OU.getAllObjectPropertyAssertions(m)
      .filter(objPropAssertionInvolvesDFDObjs)
      .filterNot(_.getProperty.asOWLObjectProperty().getIRI.toString.contains("DataFlow"))
      .map (opax => "\n  " +
        LU.makeNameResourceNode(opax.getSubject.asOWLNamedIndividual()) +
        " -> " +
        LU.makeNameResourceNode(opax.getObject.asOWLNamedIndividual()) +
        " [shape=curve label=\"" +
        IU.getSimpleName(opax.getProperty.asOWLObjectProperty()).split("_").last +"\"]")











  private
  def discoverResourceType(n: OWLNamedIndividual): Option[String] =
  {
    OU.getAllClassAssertions(m).filter(_.getIndividual == n)
      .filter(_.getClassExpression.isClassExpressionLiteral)
      .map (_.getClassExpression.asOWLClass().getIRI.toString.split("/").last )
      .find( resType => Set("s3bucket#bucket","snstopic#topic",
        "sqsqueue#queue","apigatewayrestapi#restapi",
        "lambdafunction#function","iamrole#role") contains resType )
  }

  private
  def getLabelDependingOnRecordType(n :OWLNamedIndividual) =
  {
    if (OU.getAllDataRecordOnlyNodes(m) contains n)
      LU.makeLabelRecords(n,m)
    else if (OU.getAllSomeDataRecordNodes(m) contains n)
      LU.makeLabelRecordsWithName(n,LU.makeLabelResourceNode(n),m)
    else
      LU.makeLabelResourceNode(n)
  }


  private
  def classAssertionInvolvesDFDobjs(ax: OWLClassAssertionAxiom) =
    (ax.getClassExpression.classesInSignature().toScala(List) find (c => IU.isDFDIRI(c.getIRI)) match {
      case None => IU.isDFDIRI(ax.getIndividual.asOWLNamedIndividual().getIRI)
      case _ => true
    } ) && !ax.getClassExpression.isOWLThing

//  private
//  def classAssertionInvolvesServiceDFDobjs(ax: OWLClassAssertionAxiom) =
//    (ax.getClassExpression.classesInSignature().toScala(List) find (c => IU.isServiceDFDIRI(c.getIRI)) match {
//      case None => IU.isServiceDFDIRI(ax.getIndividual.asOWLNamedIndividual().getIRI)
//      case _ => true
//    } ) && !ax.getClassExpression.isOWLThing
//
//  private
//  def dataPropAssertionInvolvesDFDobjs(ax: OWLDataPropertyAssertionAxiom) =
//    (IU.isDFDIRI(ax.getProperty.asOWLDataProperty().getIRI) ||
//      IU.isDFDIRI(ax.getSubject.asOWLNamedIndividual().getIRI)) &&
//    !ax.getProperty.isOWLTopDataProperty
//
//  private
//  def dataPropAssertionInvolvesServiceDFDobjs(ax: OWLDataPropertyAssertionAxiom) =
//    (IU.isServiceDFDIRI(ax.getProperty.asOWLDataProperty().getIRI) ||
//      IU.isServiceDFDIRI(ax.getSubject.asOWLNamedIndividual().getIRI)) &&
//      !ax.getProperty.isOWLTopDataProperty

  private
  def objPropAssertionInvolvesDFDObjs(ax: OWLObjectPropertyAssertionAxiom) =
    (IU.isDFDIRI(ax.getObject.asOWLNamedIndividual().getIRI) ||
      IU.isDFDIRI(ax.getProperty.asOWLObjectProperty().getIRI) ||
      IU.isDFDIRI(ax.getSubject.asOWLNamedIndividual().getIRI)) &&
      !ax.getProperty.isOWLTopObjectProperty

//  private
//  def objPropAssertionInvolvesServiceDFDObjs(ax: OWLObjectPropertyAssertionAxiom) =
//    (IU.isServiceDFDIRI(ax.getObject.asOWLNamedIndividual().getIRI) ||
//      IU.isServiceDFDIRI(ax.getProperty.asOWLObjectProperty().getIRI) ||
//      IU.isServiceDFDIRI(ax.getSubject.asOWLNamedIndividual().getIRI)) &&
//      !ax.getProperty.isOWLTopObjectProperty















  private
  def getAllNodesOwnedByAccount(acc: OWLNamedIndividual) =
    m.ontologies().toScala(List).flatMap(o => {
      OU.getAllObjPropAssertionsIndividual(o,acc)
        .filter (opax => IU.getSimpleName(opax.getProperty.asOWLObjectProperty()).endsWith("owns"))
        .flatMap(opax => List(opax.getObject.asOWLNamedIndividual()))
    })

  private
  def nodesInvolvedInDFDPropAssertions: Set[OWLNamedIndividual] =
    OU.getAllObjectPropertyAssertions(m)
      .filter( x => objPropAssertionInvolvesDFDObjs(x))
      .flatMap( x => Set(x.getObject.asOWLNamedIndividual(), x.getSubject.asOWLNamedIndividual()))

//  private
//  def isDFDcomponent (n : OWLNamedIndividual) = {
//    val iris = m.ontologies().toScala(List).flatMap( o =>
//      OU.getAllClassAssertionsIndividual(o,n)
//        .filter(classAssertionInvolvesDFDobjs)
//        .map(ca=> ca.getClassExpression.asOWLClass().getIRI)).toString
//
//    if (iris contains Ontology.VersionStringIRI+"dfd#DataStore")
//      "Storage"
//    else if (iris contains Ontology.VersionStringIRI+"dfd#Process")
//      "Process"
//    else "None"
//  }



}

