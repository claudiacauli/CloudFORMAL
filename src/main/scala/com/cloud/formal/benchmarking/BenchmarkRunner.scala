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

package com.cloud.formal.benchmarking

import java.io.{File, PrintWriter}

import com.cloud.formal.binding.Interface
import com.cloud.formal.mapping.templates.Infrastructure
import com.cloud.formal.model.Model
import com.cloud.formal.reasoning.PropertyType.PropertyType
import com.cloud.formal.reasoning.QueryOutcome.QueryOutcome
import com.cloud.formal.reasoning.{PropertiesChecker, PropertyType, QueryOutcome, ReasonerWrapper}
import com.cloud.formal.{Extension, FilePath}
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.reasoner.{InconsistentOntologyException, NodeSet, OWLReasoner}

object BenchmarkRunner
{

  private var BenchmarkResults: Vector[ModelData] = Vector()


  def run(): Unit =
  {
    val t = System.nanoTime()




    // TODO Uncomment to benchmark *ALL* Resource Specifications Encoding to OWL
    BenchmarkSpecifications()




    // TODO Uncomment to benchmark *ALL* Templates Encoding to OWL
    BenchmarkEncoding(FilePath.BenchmarksIn)




    // TODO Uncomment to benchmark *ALL* Templates OWL Classification
    // NB: Models MUST be already encoded in "BenchmarksOut/" folder.
    BenchmarkClassification()

    // TODO Uncomment to benchmark *SINGLE* Templates OWL Classification
    // NB: Models MUST be already encoded in "BenchmarksOut/" folder.
    //  BenchmarkSingleClassification("BenchmarksOut/07_rubajaj/")
    //  BenchmarkSingleClassification("BenchmarksOut/04_stationeering/")
    //  BenchmarkSingleClassification("BenchmarksOut/15_retailmenot/")
    //  BenchmarkSingleClassification("BenchmarksOut/14_joshbalfour/")
    //  BenchmarkSingleClassification("BenchmarksOut/13_apiconcord/")
    //  BenchmarkSingleClassification("BenchmarksOut/12_happypeter/")
    //  BenchmarkSingleClassification("BenchmarksOut/06_velaskec/")
    //  BenchmarkSingleClassification("BenchmarksOut/10_samuelweckstrom/")
    //  BenchmarkSingleClassification("BenchmarksOut/08_sqilupinc/")
    //  BenchmarkSingleClassification("BenchmarksOut/01_sqilup/")
    //  BenchmarkSingleClassification("BenchmarksOut/02_johnBh/")
    //  BenchmarkSingleClassification("BenchmarksOut/09_tatums/")
    //  BenchmarkSingleClassification("BenchmarksOut/03_kalyanmca13/")
    //  BenchmarkSingleClassification("BenchmarksOut/11_widdix/")
    //  BenchmarkSingleClassification("BenchmarksOut/05_monishakrish25992/")




    // TODO Uncomment to benchmark *ALL* Templates OWL Query Answering
    // NB: Models MUST be already encoded in folder BenchmarksOut/
    BenchmarkQueryAnswering()

    // TODO Uncomment to benchmark *SINGLE* Templates OWL Query Answering
    // NB: Models MUST be already encoded in folder BenchmarksOut/
    //  BenchmarkSingleQueryAnswering("BenchmarksOut/07_rubajaj/")
    //  BenchmarkSingleQueryAnswering("BenchmarksOut/04_stationeering/")
    //  BenchmarkSingleQueryAnswering("BenchmarksOut/15_retailmenot/")
    //  BenchmarkSingleQueryAnswering("BenchmarksOut/05_monishakrish25992/")
    //  BenchmarkSingleQueryAnswering("BenchmarksOut/11_widdix/")
    //  BenchmarkSingleQueryAnswering("BenchmarksOut/03_kalyanmca13/")
    //  BenchmarkSingleQueryAnswering("BenchmarksOut/09_tatums/")
    //  BenchmarkSingleQueryAnswering("BenchmarksOut/02_johnBh/")
    //  BenchmarkSingleQueryAnswering("BenchmarksOut/01_sqilup/")
    //  BenchmarkSingleQueryAnswering("BenchmarksOut/08_sqilupinc/")
    //  BenchmarkSingleQueryAnswering("BenchmarksOut/10_samuelweckstrom/")
    //  BenchmarkSingleQueryAnswering("BenchmarksOut/06_velaskec/")
    //  BenchmarkSingleQueryAnswering("BenchmarksOut/12_happypeter/")
    //  BenchmarkSingleQueryAnswering("BenchmarksOut/13_apiconcord/")
    //  BenchmarkSingleQueryAnswering("BenchmarksOut/14_joshbalfour/")



    //println(" Benchmark took " + (System.nanoTime()-t)/Math.pow(10,6) + " ms.")



    // Prints Latex Table's Rows from the .csv file to the console
    LatexTablePrinter.print()
  }























  private class QueryData(id:String, val pType:PropertyType)
  {
    var satTime: Long = _
    var compoundQueryTime: Long = _
    var queryOutcome: QueryOutcome = _

    override def toString: String =
      s"\n\t$id(pType: $pType, satTime: $satTime, queryTime: $compoundQueryTime,"+
      s" queryOutcome: $queryOutcome) "

  }


  private class ModelData(val infrastrName:String, val resN:Int, resTypeN: Int,
                          val logicalAxsN:Int, encodingTime:Long)
  {
    var classificationTime: Long = _
    var queriesData: Vector[QueryData] = Vector()

    override def toString: String =
      s" Infrastructure $infrastrName - res#: $resN, resType#: $resTypeN,"+
      s" encodingTime: $encodingTime, logicalAxioms#: $logicalAxsN, "+
      s"classificationTime: $classificationTime" +
      queriesData.foldLeft("")((a,qd) => a+qd)


    def updateCsvQueryTimes(): Unit = {

      def getQueries(t1: PropertyType, t2: PropertyType, o: QueryOutcome): Vector[QueryData] =
        queriesData.filter(q => (q.pType==t1 || q.pType==t2) && q.queryOutcome==o)

      def q13u: Vector[QueryData] = getQueries(PropertyType.TFF,PropertyType.FTT,QueryOutcome.UNSAT)
      def q130: Vector[QueryData] = getQueries(PropertyType.TFF,PropertyType.FTT,QueryOutcome.SAT0)
      def q131: Vector[QueryData] = getQueries(PropertyType.TFF,PropertyType.FTT,QueryOutcome.SAT1)

      def avg(qs: Vector[QueryData]): Long =
        if (qs.isEmpty) 0L
        else qs.foldLeft(0L)((a,i)=> a+i.compoundQueryTime)/qs.size

      def ms(l : Long): String =
        if (l==0L) "--"
        else f"${l/Math.pow(10,6)}%.2f"

      updateCSVwQueryData(infrastrName.split("_").head,q13u,q130,q131)(ms)(avg)
    }

  }






  private def BenchmarkSpecifications(): (Long, Unit) =
    BenchmarkUtils.ProfileFunction("Specification Encoding",
      BenchmarkUtils.time(Interface.compileAndSaveSpecification(FilePath.ResourceSpecs, printEnabled = false)),
      BenchmarkUtils.warmUp(Interface.compileAndSaveSpecification(FilePath.ResourceSpecs, printEnabled = false)))


  private def BenchmarkEncoding(filePath: String)
  : Vector[(String, OWLOntology, OWLDataFactory, OWLOntologyManager)] =
    Interface.modelAndSaveAllTemplates(filePath, FilePath.BenchmarksOut, createAndBenchmarkInfrastructure)


  private def BenchmarkSingleClassification(inPath: String): Unit =
    BenchmarkClassification(inPath)



  private def BenchmarkClassification(inPath: String = null): Unit =
  {
    val vm =  if (inPath==null) loadModelsAndCreateModelData()
    else loadSingleModelAndCreateModelData(inPath)

    val zipped =
      if (vm.size==1) vm zip BenchmarkResults.filter(md => md.infrastrName==vm.head._1)
      else vm zip BenchmarkResults

    zipped.foreach( p =>
      {
        val r = ReasonerWrapper.create(p._1._2, p._1._3, p._1._4)
        try {
          r.classify(printEnabled = false)(computeAllInferencesAndBenchmark(p._2, r))
          val pc = new PropertiesChecker(p._1._1, p._1._2, p._1._3, p._1._4,".")
          pc.classify(printEnabled = false)
          println(s"\tAxioms: ${p._2.logicalAxsN}")
        }
        catch {
          case _: InconsistentOntologyException
          => println("\tINCONSISTENT ONTOLOGY FOUND. SKIPPING. ")
        }
      })
  }


  private def BenchmarkSingleQueryAnswering(inPath: String): Unit = {
    BenchmarkQueryAnswering(inPath)
  }



  private def BenchmarkQueryAnswering(inPath: String = null): Unit =
  {

    val vm = if (inPath==null) loadModelsAndCreateModelData()
              else loadSingleModelAndCreateModelData(inPath)

    val zipped =
      if (vm.size==1) vm zip BenchmarkResults.filter(md => md.infrastrName==vm.head._1)
      else vm zip BenchmarkResults

    zipped.foreach( p =>
      {
        val r = ReasonerWrapper.create(p._1._2, p._1._3, p._1._4)
        try {
          r.classify(printEnabled = false)(r.computeAllInferences)
          val pc = new PropertiesChecker(p._1._1, p._1._2, p._1._3, p._1._4,".")
          pc.classify(printEnabled = false)
          println(s"\tAxioms: ${p._2.logicalAxsN}")
          println(s"[Benchmarking Query Answering ${p._1._1}]")
          p._2.queriesData =
            pc.propsVec.sortBy(_._2.id)flatMap(
              pr =>
              {
                val qd = new QueryData(pr._2.id, pr._2.propType)
                if (pc.pE.hasRequiredResourceTypes(pc.pE.o, pr._2))
                {
                  pc.runEach(pr._2, printEnabled = false)(runAndBenchmarkQuery(qd, pc.r))(pc.r.isSat)
                  //println(f"\t${pr._2.id}\t\t\t\t\t${qd.compoundQueryTime/Math.pow(10, 6)}%.2f ms \t  ${pc.pE.makeQuery(pr._2)}")
                  Vector(qd)
                }
                else
                {
                  //println("\t" + pr._2.id + "\t\t" + " N/A")
                  Vector()
                }
              })
          p._2.updateCsvQueryTimes()
        }
        catch {
          case _: InconsistentOntologyException
          => println("\tINCONSISTENT ONTOLOGY FOUND. SKIPPING. ")
        }
      })
    println("\n\n\n")
  }



  private def loadSingleModelAndCreateModelData(inPath: String)
  : Vector[(String,OWLOntology,OWLDataFactory,OWLOntologyManager)] = {
    val dir = new File(inPath)
        val f = dir.listFiles().filter(_.getName.endsWith(Extension.Owl)).head
        val im = Interface.loadModel(f.getAbsolutePath, printEnabled = false)
        BenchmarkResults ++= Vector(
          new ModelData(im._4,resN = 0, resTypeN = 0,
            im._1.getLogicalAxiomCount(Imports.INCLUDED), encodingTime = 0))
        Vector((im._4,im._1,im._2,im._3))
  }



  private def loadModelsAndCreateModelData()
  : Vector[(String,OWLOntology,OWLDataFactory,OWLOntologyManager)] = {
    new File(FilePath.BenchmarksOut)
      .listFiles().filter(_.isDirectory).sortBy(_.getName)
      .map( dir => {
        val f = dir.listFiles().filter(_.getName.endsWith(Extension.Owl)).head
        val im = Interface.loadModel(f.getAbsolutePath, printEnabled = false)
        BenchmarkResults ++= Vector(
          new ModelData(im._4,resN = 0, resTypeN = 0,
            im._1.getLogicalAxiomCount(Imports.INCLUDED), encodingTime = 0))
        (im._4,im._1,im._2,im._3)
      }).toVector
  }






  private def runAndBenchmarkQuery(qd: QueryData, r: ReasonerWrapper)
                                  (satFun: (=> OWLClassExpression) => Boolean)
                                  (expr: => OWLClassExpression):
  (QueryOutcome, Option[NodeSet[OWLNamedIndividual]]) =
  {
    val res = BenchmarkUtils.ProfileTwoFunctions(satFun(expr), r.hasInstances(expr), r.unsatOutcome)
    qd.compoundQueryTime = res._2
    qd.queryOutcome = res._3._1
    res._3
  }


  private def computeAllInferencesAndBenchmark(md: ModelData, r: ReasonerWrapper)
                                              (re: OWLReasoner) :Unit =
  {
    val res = BenchmarkUtils.ProfileFunction("Classification "+md.infrastrName,
      BenchmarkUtils.timePreFun(r.jFactReasoner(),r.computeAllInferences),
      BenchmarkUtils.warmUpPreFun(r.jFactReasoner(),r.computeAllInferences))
    println("Mean time " + res._1)
    md.classificationTime = res._1

  updateCSVwClassificationData(md.infrastrName.split("_").head,md)

  }



  private def createAndBenchmarkInfrastructure(file: File, inputPath: String, outputPath: String)
  : (String, OWLOntology, OWLDataFactory, OWLOntologyManager) =
  {
    val infrastrName = file.getName
    val p = BenchmarkUtils.ProfileFunction("Template Encoding "+infrastrName,
      BenchmarkUtils.time(Interface.encodeInfrastructure(printEnabled = false)(file,infrastrName, inputPath)),
      BenchmarkUtils.warmUp(Interface.encodeInfrastructure(printEnabled =false)(file,infrastrName, inputPath)))
    val i : Infrastructure = p._2._1
    val im = p._2._2
    im.writeToOutputFolder(outputPath)

    println(s"ResN: ${i.getResourcesCount}\tResTypesN: ${i.getResourceTypesCount}")

    BenchmarkResults ++= Vector(
      new ModelData(infrastrName,i.getResourcesCount,i.getResourceTypesCount,
        im.ontology.getLogicalAxiomCount(Imports.INCLUDED),p._1))

    //println("The Infrastructure is : " + infrastrName )

    updateCSVwInfrastructureData(infrastrName.split("_").head,i,p._1,im)

    (im.name, im.ontology, im.df, im.manager)
  }


  private def updateCSVwInfrastructureData(id: String, i: Infrastructure, encTime: Long, im: Model): Unit =
    updateCSVLineWithFunction(id,encFun(i,im,encTime))



  private def updateCSVwClassificationData(id: String, md: ModelData) : Unit =
    updateCSVLineWithFunction(id,classFun(md))


  private def updateCSVwQueryData(id: String, q13u: Vector[QueryData], q130: Vector[QueryData], q131: Vector[QueryData])
                         (ms: Long => String)(avg: Vector[QueryData] => Long): Unit =
    updateCSVLineWithFunction(id, queryFun(ms,avg,q13u,q130,q131))










  private def updateCSVLineWithFunction(id: String, fun: Array[String] => Unit) : Unit = {
    val csvFile = getCsvFile()
    val bs = io.Source.fromFile(csvFile)
    var newStr = ""
    for (line <- bs.getLines()){
      if (line.nonEmpty){
        if (line.split(",").head == id) {
          val values = line.split(",",-1)
          fun(values)
          newStr += values.mkString(",")+"\n"
        } else {
          newStr += line+"\n"
        }
      }
    }
    bs.close()
    overwriteCsvWithString(newStr)
  }


  private def queryFun(ms: Long => String, avg: Vector[QueryData] => Long,
                       q13u: Vector[QueryData], q130: Vector[QueryData], q131: Vector[QueryData])(values : Array[String]) : Unit =
  {
    values(CSV.Usat) = ms(avg(q13u))
    values(CSV.Sat0) = ms(avg(q130))
    values(CSV.Sat1) = ms(avg(q131))
  }


  private def classFun (md: ModelData)(values : Array[String]) : Unit =
    values(CSV.ClassT) = f"${md.classificationTime/Math.pow(10,6)}%.2f"


  private def encFun (i:Infrastructure, im: Model, encTime: Long)(values: Array[String]) : Unit =
  {
    values(CSV.Nr) = i.getResourcesCount.toString
    values(CSV.Nrt) = i.getResourceTypesCount.toString
    values(CSV.EncT) = f"${encTime/Math.pow(10,6)}%.2f"
    values(CSV.Naxs) = im.ontology.getLogicalAxiomCount(Imports.INCLUDED).toString
  }



  private def overwriteCsvWithString(s: String): Unit =
  {
    val csvFile = getCsvFile()
    val pw = new PrintWriter(csvFile)
    pw.write(s)
    pw.close()
  }


  private def getCsvFile(): File =
    if (!new File(CSV.FilePath).exists())
    {
      val f = new File(CSV.FilePath)
      val pw = new PrintWriter(f)
      pw.write(BenchmarkResults.foldLeft("")( (a,md) =>
        a + "\n" + md.infrastrName.split("_")(0)) + ",-,-,-,-,-,-,-,-")
      pw.close()
      f
    }
    else
      new File(CSV.FilePath)


}
