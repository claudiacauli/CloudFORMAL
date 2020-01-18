package com.cloud.formal

import java.io.{File, FileNotFoundException}

import argonaut.{Json, Parse}
import com.cloud.formal.binding.Interface
import com.cloud.formal.binding.Interface.{compileAndSaveTemplates, createDescriptorForTemplateName, encodeInfrastructure}
import com.cloud.formal.mapping.templates.{Infrastructure, Json2InfrastructureEncoder}
import com.cloud.formal.mapping.templates.mapping.InfrastructureModel
import com.cloud.formal.reasoning.PropertyType.PropertyType
import com.cloud.formal.reasoning.QueryOutcome.QueryOutcome
import org.semanticweb.owlapi.model.parameters.Imports

import scala.Console.{BLUE, RESET}
import scala.io.Source


object BenchmarkRunner extends App {





  class QueryData(id:String, pType:PropertyType, satTime:Long,
                          totalQueryTime:Long, outcome:QueryOutcome,
                          tfuOutcome:String)

  class ModelData(infrastrName:String, resN:Int, resTypeN: Int, logicalAxsN:Int,
                          encodingTime:Long, classificationTime:Long,
                          queriesData:Vector[QueryData])

  type BenchmarkSet = Vector[ModelData]







  // classify it

  Interface.modelAndSaveAllTemplates(Array{"Benchmarks/"}, createAndBenchmarkInfrastructure)


  private def createAndBenchmarkInfrastructure(file: File, inputPath: String, outputPath: String) = {

    val infrastrName = file.getName

    println("\n "+ infrastrName)

    val p = BenchmarkUtils.timeN(20, 100)("Encoding", Interface.encodeInfrastructure(file,infrastrName, inputPath))
    val encodingTime = p._1
    val i : Infrastructure = p._2._1
    val im = p._2._2
    val resN = i.getResourcesCount
    val resTypesN = i.getResourceTypesCount
    im.writeToOutputFolder(outputPath)
    val logicalAxsN = im.ontology.getLogicalAxiomCount(Imports.INCLUDED)

    println(s" - [Encoding] Resources count: $resN")
    println(s" - [Encoding] Resource types count: $resTypesN")
    println(s" - [Encoding] Logical Axioms count: $logicalAxsN")
    println(f" - [Encoding] Mean time: $encodingTime ns (${encodingTime/Math.pow(10,6)}%.2f ms, ${encodingTime/Math.pow(10,9)}%.2f s).")

    (im.name, im.ontology, im.df, im.manager)
  }













  private[formal]
  object BenchmarkUtils
  {





    def timeN[R](wn: Int, n: Int)(funName: String, fun: => R): (Long,R) = {

      def time[R](i: Int, funName: String, fun: => R): (Long,R) = {
        val t0 = System.nanoTime()
        val res = fun
        val t1 = System.nanoTime()
        //println(s" $i $funName Elapsed time: ${t1-t0} ns." )
        //if (i%100==0) println(s"$i-th iteration")
        (t1-t0,res)
      }

      def warmUp =
        (1 to wn).foreach(i => time(i,funName,fun))

      warmUp
      val res = (1 to n).map(i => {
        System.gc()
        System.runFinalization()
        time(i,funName,fun)
      })
      val meanTime = res.foldLeft(0L)((a,e) => a + e._1)/res.size
      //println(s"$BLUE - [$funName]$RESET Elapsed mean time: $BLUE$meanTime ns (${meanTime/Math.pow(10,6)} ms).$RESET")
      (meanTime,res.last._2)
    }







    def timeNwPreFun[R,S](wn:Int, n: Int)(funName: String, preFun: => S, fun: S => R): (Long,R) = {

      def time[R](i: Int, funName: String, preFun: => S, fun: S => R): (Long,R) = {
        val outPrefun = preFun
        val t0 = System.nanoTime()
        val res = fun(outPrefun)
        val t1 = System.nanoTime()
        //println(s" $i $funName Elapsed time: ${t1-t0} ns." )
        //if (i%100==0) println(s"$i-th iteration")
        (t1-t0,res)
      }

      def warmUp =
        (1 to wn).foreach(i => time(i,funName,preFun,fun))

      warmUp
      val res = (1 to n).map(i => {
        System.gc()
        System.runFinalization()
        time(i,funName, preFun, fun)
      })
      val meanTime = res.foldLeft(0L)((a,e) => a + e._1)/res.size
      //println(s"$BLUE - [$funName]$RESET Elapsed mean time: $BLUE$meanTime ns (${meanTime/Math.pow(10,6)} ms).$RESET")
      (meanTime,res.last._2)
    }




  }




}