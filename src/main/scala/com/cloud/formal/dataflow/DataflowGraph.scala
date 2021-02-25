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

import java.io.{File, FileOutputStream}

import com.cloud.formal.{FilePath, Ontology, Extension => Ex}
import com.cloud.formal.dataflow.{LabelsUtils => LU}
import com.cloud.formal.dataflow.{OntologyUtils => OU}
import com.cloud.formal.dataflow.{IRIUtils => IU}
import com.cloud.formal.model.ModelFileSuffix
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{AddImport, OWLDataFactory, OWLNamedIndividual, OWLObjectPropertyAssertionAxiom, OWLOntology, OWLOntologyAlreadyExistsException, OWLOntologyManager}

import scala.jdk.StreamConverters._
import Console.{BOLD, RESET}


object DataflowGraph extends Graph {

  val dotFileName: String = DFDNaming.DFDTAG + Ex.DOT
  val graphFileName: String = DFDNaming.DFDTAG + Ex.PNG
  var o: OWLOntology = _
  var df: OWLDataFactory = _
  var m: OWLOntologyManager = _


  def run(path: String): Unit =
  {

    val pathFile = new File (path)
    if (pathFile.isDirectory && pathFile.getName.contains(DFDNaming.DFDTAG)){
      val file = pathFile.listFiles().find(_.getName.endsWith(Ex.OWL)).get
      runFromDFDModel(file)
    } else if (pathFile.isDirectory){
      val file = pathFile.listFiles().find(_.getName.endsWith(ModelFileSuffix.Infrastructure)).get
      runFromInfrastructureModel(file)
    } else if (pathFile.getName.endsWith(ModelFileSuffix.Dataflow))
      runFromDFDModel(pathFile)
    else if (pathFile.getName.endsWith(ModelFileSuffix.Infrastructure))
      runFromInfrastructureModel(pathFile)
    else println("No Model file found (neither Infrastructure nor DFD Model.")

  }


  private
  def runFromInfrastructureModel(infrastructureModelFile: File) =
  {
    var outPath = infrastructureModelFile.getAbsolutePath.split("/").dropRight(1).mkString("/")
    var inPath = infrastructureModelFile.getAbsolutePath

    if (!outPath.endsWith("/"))
      outPath = outPath + "/"

    println(s"\n$BOLD Building DFD of " + inPath.split("/").last + s"$RESET")

    initializeGraphGenerator(inPath)

    println("  Initializing Graph Generator")

    loadAllOntologiesNeededForDFD()
    println("  Loading DFD Ontologies")
    val reasEngine = OU.startReasoner(o,df,m)

    println("  Starting Reasoner Engine")
    val t = System.currentTimeMillis()
    OU.addInferredAxiomsToMainOntologyManager(o,m,reasEngine)

    //    println("The main ontology now imports: ")
    //    o.imports().forEach(println)

    saveReasonedDFDOntology(inPath, outPath)

    println("  Merged Inferred Axioms into Infrastructure Model \t[" + (System.currentTimeMillis()-t) + " ms].")
    printGraphToDotFile(dotFileName,buildGraphDotSpecification,outPath)
    runGraphvizDotToImage(dotFileName,graphFileName, outPath)
    println(s"$BOLD DFD.png written to path: " + new File(outPath).getAbsolutePath + s"$RESET\n")
  }



  /*
  This functionality DOES NOT really work right now :(
   */
  private
  def runFromDFDModel(dfdModelFile: File) =
  {
    var outPath = dfdModelFile.getAbsolutePath.split(ModelFileSuffix.Dataflow).head
    var inPath = dfdModelFile.getAbsolutePath

    if (!outPath.endsWith("/"))
      outPath = outPath + "/"

    m   = OWLManager.createOWLOntologyManager()
    df  = m.getOWLDataFactory
    o = m.loadOntologyFromOntologyDocument(dfdModelFile)

    //println("Imported ontologies are: ")
    o.imports().forEach(println)

    printGraphToDotFile(dotFileName,buildGraphDotSpecification,outPath)
    runGraphvizDotToImage(dotFileName,graphFileName, outPath)
  }


  private
  def saveReasonedDFDOntology(inPath: String, outPath: String): Unit =
  {
    val name = inPath.split("/").last
      .split(ModelFileSuffix.Infrastructure)(0)

    val dir = new File(outPath + DFDNaming.DFDTAG + "/")
    if (!dir.exists())  dir.mkdir()
    val file = new File(outPath + DFDNaming.DFDTAG +"/" + name + ModelFileSuffix.Dataflow)
    val fos = new FileOutputStream( file )
    m.saveOntology(o, fos)
    fos.close()
  }




  private
  def loadAllOntologiesNeededForDFD(): Unit =
  {

    val resTypes = findAllResourceTypes()

    //println("Rest types are " + resTypes)

    var usedServicesWithDFDavailable =
      Ontologies.serviceTypeToDFD.keys.filter(k =>
      resTypes.exists(_.startsWith(k)))

    if (usedServicesWithDFDavailable.toVector.contains("cloudwatch"))
      usedServicesWithDFDavailable = usedServicesWithDFDavailable ++ Set("logs")

    //println("And used services with dFD support: " + usedServicesWithDFDavailable)

    new File(FilePath.DataflowResSpecs)
    .listFiles().filter(_.getName.endsWith(Ex.OWL))
      .filter( f => usedServicesWithDFDavailable.exists(s => f.getName.startsWith(s)))
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

    (List("dfd.owl") ++ usedServicesWithDFDavailable
      .map(k => Ontologies.serviceTypeToDFD(k)))
      .foreach( s => {
      val imported = m.loadOntologyFromOntologyDocument(new File(FilePath.DataflowSpecs + s))
      m.applyChange(new AddImport(o, df.getOWLImportsDeclaration( m.getOntologyDocumentIRI(imported) )))
    })


  }



  private
  def findAllResourceTypes() : Vector[String] = {

    var resTypes: Vector[String] = Vector()

    o.imports().filter(o => {
      !o.getOntologyID.getOntologyIRI.get().getIRIString.endsWith("aws#") &&
      !o.getOntologyID.getOntologyIRI.get().getIRIString.endsWith("stackset#")
    }).forEach( o => resTypes = resTypes ++ List(o.getOntologyID.getOntologyIRI.get().toString.split("/").last.replace("#","")))

      resTypes
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





//  private
//  def classAssertionInvolvesDFDobjs(ax: OWLClassAssertionAxiom) =
//    (ax.getClassExpression.classesInSignature().toScala(List) find (c => IU.isDFDIRI(c.getIRI)) match {
//      case None => IU.isDFDIRI(ax.getIndividual.asOWLNamedIndividual().getIRI)
//      case _ => true
//    } ) && !ax.getClassExpression.isOWLThing





  private
  def objPropAssertionInvolvesDFDObjs(ax: OWLObjectPropertyAssertionAxiom) =
    (IU.isDFDIRI(ax.getObject.asOWLNamedIndividual().getIRI) ||
      IU.isDFDIRI(ax.getProperty.asOWLObjectProperty().getIRI) ||
      IU.isDFDIRI(ax.getSubject.asOWLNamedIndividual().getIRI)) &&
      !ax.getProperty.isOWLTopObjectProperty





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





}
