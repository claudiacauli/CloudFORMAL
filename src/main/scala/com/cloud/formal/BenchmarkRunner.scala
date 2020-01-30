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

package com.cloud.formal

import java.io.File

import com.cloud.formal.binding.Interface
import com.cloud.formal.mapping.templates.Infrastructure
import com.cloud.formal.reasoning.PropertyType.PropertyType
import com.cloud.formal.reasoning.QueryOutcome.QueryOutcome
import com.cloud.formal.reasoning.{PropertiesChecker, PropertyType, QueryOutcome, Reasoner}
import org.semanticweb.owlapi.model.{OWLClassExpression, OWLDataFactory, OWLNamedIndividual, OWLOntology, OWLOntologyManager}
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.reasoner.{InconsistentOntologyException, NodeSet, OWLReasoner}


object BenchmarkRunner extends App {


  private var benchmarkResults: Vector[ModelData] = Vector()
  private var WN :Int = 100
  private var N  :Int = 10


  // TODO Uncomment to benchmark Resource Specifications Encoding to OWL
  // BenchmarkSpecifications()

  // TODO Uncomment to benchmark Templates Encoding to OWL
  //  BenchmarkSingleEncoding("Benchmarks/16_retailmenot/")
  //  BenchmarkEncoding()


  // TODO Uncomment to benchmark Templates OWL Classification
  // NB: Models MUST be already encoded in folder BenchmarksOut/ !!!
  //  BenchmarkSingleClassification("BenchmarksOut/07_rubajaj/")
  //    BenchmarkSingleClassification("BenchmarksOut/04_stationeering/")
//      BenchmarkSingleClassification("BenchmarksOut/16_retailmenot/")
//      BenchmarkSingleClassification("BenchmarksOut/15_joshbalfour/")
//  BenchmarkSingleClassification("BenchmarksOut/14_apiconcord/")
//  BenchmarkSingleClassification("BenchmarksOut/13_happypeter/")
//  BenchmarkSingleClassification("BenchmarksOut/06_velaskec/")
//  BenchmarkSingleClassification("BenchmarksOut/10_samuelweckstrom/")
//  BenchmarkSingleClassification("BenchmarksOut/08_sqilupinc/")
//  BenchmarkSingleClassification("BenchmarksOut/01_sqilup/")
//  BenchmarkSingleClassification("BenchmarksOut/02_johnBh/")
//  BenchmarkSingleClassification("BenchmarksOut/09_tatums/")
//  BenchmarkSingleClassification("BenchmarksOut/03_kalyanmca13/")
//  BenchmarkSingleClassification("BenchmarksOut/12_widdix/")
//  BenchmarkSingleClassification("BenchmarksOut/11_naveenkumarhm/")
//  BenchmarkSingleClassification("BenchmarksOut/05_monishakrish25992/")



  //  BenchmarkClassification()

  // TODO Uncomment to benchmark Templates OWL Query Answering
  // NB: Models MUST be already encoded in folder BenchmarksOut/

    BenchmarkSingleQueryAnswering("BenchmarksOut/05_monishakrish25992/")
    BenchmarkSingleQueryAnswering("BenchmarksOut/11_naveenkumarhm/")
    BenchmarkSingleQueryAnswering("BenchmarksOut/12_widdix/")
    BenchmarkSingleQueryAnswering("BenchmarksOut/03_kalyanmca13/")
    BenchmarkSingleQueryAnswering("BenchmarksOut/09_tatums/")
    BenchmarkSingleQueryAnswering("BenchmarksOut/02_johnBh/")
    BenchmarkSingleQueryAnswering("BenchmarksOut/01_sqilup/")
    BenchmarkSingleQueryAnswering("BenchmarksOut/08_sqilupinc/")
    BenchmarkSingleQueryAnswering("BenchmarksOut/10_samuelweckstrom/")
    BenchmarkSingleQueryAnswering("BenchmarksOut/06_velaskec/")
    BenchmarkSingleQueryAnswering("BenchmarksOut/13_happypeter/")
    BenchmarkSingleQueryAnswering("BenchmarksOut/14_apiconcord/")
    BenchmarkSingleQueryAnswering("BenchmarksOut/15_joshbalfour/")
    BenchmarkSingleQueryAnswering("BenchmarksOut/16_retailmenot/")
    BenchmarkSingleQueryAnswering("BenchmarksOut/04_stationeering/")
    BenchmarkSingleQueryAnswering("BenchmarksOut/07_rubajaj/")


  // BenchmarkQueryAnswering()















  private def printLatexTable(): Unit = {
    benchmarkResults.sortBy(_.logicalAxsN).foreach(br => println(br.toLatexTableRow))
  }








  private class QueryData(id:String, val pType:PropertyType)
  {
    var satTime: Long = _
    var compoundQueryTime: Long = _
    var queryOutcome: QueryOutcome = _

    override def toString: String =
      s"\n\t$id(pType: $pType, satTime: $satTime, queryTime: $compoundQueryTime,"+
      s" queryOutcome: $queryOutcome) "

    def toLatexTableRow: String =
      f" & ${satTime/Math.pow(10,6)}%.2f & ${compoundQueryTime/Math.pow(10,6)}%.2f & $queryOutcome"

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


    def toLatexTableRow: String = {

      def getQueries(t1: PropertyType, t2: PropertyType, o: QueryOutcome) =
        queriesData.filter(q => (q.pType==t1 || q.pType==t2) && q.queryOutcome==o)

      def q13u: Vector[QueryData] = getQueries(PropertyType.TFF,PropertyType.FTT,QueryOutcome.UNSAT)
      def q130: Vector[QueryData] = getQueries(PropertyType.TFF,PropertyType.FTT,QueryOutcome.SAT0)
      def q131: Vector[QueryData]= getQueries(PropertyType.TFF,PropertyType.FTT,QueryOutcome.SAT1)
      def q24u: Vector[QueryData]= getQueries(PropertyType.TTF,PropertyType.FFT,QueryOutcome.UNSAT)
      def q240: Vector[QueryData]= getQueries(PropertyType.TTF,PropertyType.FFT,QueryOutcome.SAT0)
      def q241: Vector[QueryData]= getQueries(PropertyType.TTF,PropertyType.FFT,QueryOutcome.SAT1)

      def avg(qs: Vector[QueryData]): Long =
        if (qs.isEmpty) 0L
        else qs.foldLeft(0L)((a,i)=> a+i.compoundQueryTime)/qs.size

      def ms(l : Long): String =
        if (l==0L) "--$\\ \\ $"
        else f"${l/Math.pow(10,6)}%.2f"

      def propsLine =
        f"& "+ms(avg(q13u))+" & "+ms(avg(q130))+" & "+ms(avg(q131))+" " +
        f"& "+ms(avg(q24u))+" & "+ms(avg(q240))+" & "+ms(avg(q241))+" \\\\ "

      def bold(in: String) =
        "\\textbf{"+in+"}"

      f" ${bold(infrastrName.split("_").head)} & $resN & $resTypeN & ${encodingTime/Math.pow(10,6)}%.2f " +
        f"& $logicalAxsN & ${classificationTime/Math.pow(10,6)}%.2f " + propsLine

    }
  }







  private def BenchmarkSpecifications(): (Long, Unit) =
    BenchmarkUtils.ProfileFunction("Specification Encoding",
      BenchmarkUtils.time(Interface.compileAndSaveSpecification(FilePath.ResourceSpecs, printEnabled = false)),
      BenchmarkUtils.warmUp(Interface.compileAndSaveSpecification(FilePath.ResourceSpecs, printEnabled = false)))



  private def BenchmarkEncoding()
  : Vector[(String, OWLOntology, OWLDataFactory, OWLOntologyManager)] =
    Interface.modelAndSaveAllTemplates(FilePath.BenchmarksIn, FilePath.BenchmarksOut, createAndBenchmarkInfrastructure)


  private def BenchmarkSingleEncoding(dirPath: String) =
    Interface.modelAndSaveTemplates(dirPath, FilePath.BenchmarksOut, createAndBenchmarkInfrastructure)


  private def BenchmarkSingleClassification(inPath: String) = {
    BenchmarkClassification(inPath)
  }


  private def BenchmarkClassification(inPath: String = null): Unit =
  {
    val vm =  if (inPath==null) loadModelsAndCreateModelData()
    else loadSingleModelAndCreateModelData(inPath)

    (vm zip benchmarkResults)
      .foreach( p =>
      {
        val r = Reasoner.create(p._1._2, p._1._3, p._1._4)
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


  private def BenchmarkSingleQueryAnswering(inPath: String) = {
    BenchmarkQueryAnswering(inPath)
  }



  private def BenchmarkQueryAnswering(inPath: String = null): Unit =
  {

    val vm = if (inPath==null) loadModelsAndCreateModelData()
              else loadSingleModelAndCreateModelData(inPath)

    (vm zip benchmarkResults)
      .foreach( p =>
      {
        val r = Reasoner.create(p._1._2, p._1._3, p._1._4)
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
                  println(f"\t${pr._2.id}\t\t\t\t\t${qd.compoundQueryTime/Math.pow(10, 6)}%.2f ms \t  ${pc.pE.makeQuery(pr._2)}")
                  Vector(qd)
                }
                else
                {
                  println("\t" + pr._2.id + "\t\t" + " N/A")
                  Vector()
                }
              })
          println(p._2.toLatexTableRow+"\n")
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
        benchmarkResults ++= Vector(
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
        benchmarkResults ++= Vector(
          new ModelData(im._4,resN = 0, resTypeN = 0,
            im._1.getLogicalAxiomCount(Imports.INCLUDED), encodingTime = 0))
        (im._4,im._1,im._2,im._3)
      }).toVector
  }



//  private def BenchmarkClassificationAndQueryAnswering(): Unit =
//  {
//
//    val vm = loadModelsAndCreateModelData()
//
//    (vm zip benchmarkResults)
//      .foreach( p =>
//      {
//        val r = Reasoner.create(p._1._2, p._1._3, p._1._4)
//        try {
//          r.classify(printEnabled = false)(computeAllInferencesAndBenchmark(p._2, r))
//          val pc = new PropertiesChecker(p._1._1, p._1._2, p._1._3, p._1._4)
//          pc.classify(printEnabled = false)
//          println(s"\tAxioms: ${p._2.logicalAxsN}")
//          println(s"[Benchmarking Query Answering ${p._1._1}]")
//          p._2.queriesData =
//            pc.init().sortBy(_._2.id)flatMap(
//              pr =>
//              {
//                val qd = new QueryData(pr._2.id, pr._2.propType)
//                pc.runEach(pr._2, printEnabled = false)(runAndBenchmarkQuery(qd, pc.r))(pc.r.isSat)
//                if (pc.pE.hasRequiredResourceTypes(pc.pE.o, pr._2))
//                {
//                  println("\t" + pr._2.id + "\t\t" + (qd.compoundQueryTime / Math.pow(10, 6)) + " ms")
//                  Vector(qd)
//                }
//                else
//                {
//                  println("\t" + pr._2.id + "\t\t" + " N/A")
//                  Vector()
//                }
//              })
//          println(p._2.toLatexTableRow+"\n")
//        }
//        catch {
//          case _: InconsistentOntologyException
//          => println("\tINCONSISTENT ONTOLOGY FOUND. SKIPPING. ")
//        }
//      })
//    println("\n\n\n")
//    printLatexTable()
//  }



  private def runAndBenchmarkQuery(qd: QueryData, r: Reasoner)
                                  (satFun: (=> OWLClassExpression) => Boolean)
                                  (expr: => OWLClassExpression):
  (QueryOutcome, Option[NodeSet[OWLNamedIndividual]]) =
  {
    val res = BenchmarkUtils.ProfileTwoFunctions(satFun(expr), r.hasInstances(expr), r.unsatOutcome)
    //qd.satTime = res._1
    qd.compoundQueryTime = res._2
    qd.queryOutcome = res._3._1
    res._3
  }


  private def computeAllInferencesAndBenchmark(md: ModelData, r: Reasoner)
                                              (re: OWLReasoner) :Unit =
  {
    val res = BenchmarkUtils.ProfileFunction("Classification "+md.infrastrName,
      BenchmarkUtils.timePreFun(r.jFactReasoner(),r.computeAllInferences),
      BenchmarkUtils.warmUpPreFun(r.jFactReasoner(),r.computeAllInferences))
    println("Mean time " + res._1)
    md.classificationTime = res._1
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

    benchmarkResults ++= Vector(
      new ModelData(infrastrName,i.getResourcesCount,i.getResourceTypesCount,
        im.ontology.getLogicalAxiomCount(Imports.INCLUDED),p._1))
    (im.name, im.ontology, im.df, im.manager)
  }













  private object BenchmarkUtils
  {



    def ProfileFunction[R](name: String, timeFun: => (Long,R), warmUpFun: => Unit): (Long,R) =
    {
      println(s"\n[Benchmarking $name]")
      warmUpFun
      System.gc()
      System.runFinalization()
      val res = (1 to N).map(i => {
        if (i!=N && i%50==0){
          System.gc()
          System.runFinalization()
          //if (i%50==0) println("\tMeasured Iteration n " + i)
        }
        timeFun
      })
      val meanTime = res.foldLeft(0L)((a,e) => a + e._1)/res.size
      //println(s"\tMean Time: $meanTime")
      (meanTime,res.last._2)
    }



    def ProfileTwoFunctions[R](fun1: => Boolean,
                            fun2: => R, fun3: => R)
    :(Long,Long,R) = {

      def time(fun1: => Boolean, fun2: => R, fun3: R):
      (Long,Long,R) =
      {
        val t0 = System.nanoTime()
        val res1 = fun1
        //val t1 = System.nanoTime()
        val res2: R = if (res1) fun2 else fun3
        (0,System.nanoTime()-t0,res2)
      }

      def warmUp() :Unit =
        (1 to WN).foreach(i => {
          if (i%50==0){
            System.gc()
            System.runFinalization()
            //println("\tWarmup Iteration n " + i)
          }
          time(fun1,fun2, fun3)
        })
      warmUp()
      val res = (1 to N).map(i => {
        if (i!=N && i%50==0){
          System.gc()
          System.runFinalization()
          //println("\tMeasured Iteration n " + i)
        }
        time(fun1,fun2,fun3)
      })
      val meanTimeFun1 = res.foldLeft(0L)((a,e) => a + e._1)/res.size
      val meanTimeFun2 = res.foldLeft(0L)((a,e) => a + e._2)/res.size
      (meanTimeFun1, meanTimeFun2,res.last._3)
    }



    def time[R](fun: => R): (Long,R) =
    {
      val t0 = System.nanoTime()
      val res = fun
      (System.nanoTime-t0,res)
    }



    def warmUp[R](fun: => R): Unit =
      (1 to WN).foreach(i => {
        if (i!=N && i%50==0){
          System.gc()
          System.runFinalization()
          //println("\tWarmup Iteration n " + i)
        }
        time(fun)
      })



    def timePreFun[S,R](preFun: => S, fun: S => R): (Long,R) =
    {
      val outPrefun = preFun
      val t0 = System.nanoTime()
      val res = fun(outPrefun)
      val t1 = System.nanoTime()
      println(t1-t0)
      (t1-t0,res)
    }



    def warmUpPreFun[S,R](preFun: => S, fun: S => R): Unit =
      (1 to WN).foreach(i => {
        if (i!=N && i%5==0){
          System.gc()
          System.runFinalization()
          if (i%10==0) println("\tWarmup Iteration n " + i)
        }
        timePreFun(preFun,fun)
      })





  }





}