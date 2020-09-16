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

package com.cloud.formal.cli

import java.io.File

import com.cloud.formal.benchmarking.BenchmarkRunner
import com.cloud.formal.{Extension, FilePath, SysUtil}
import com.cloud.formal.binding.Interface
import com.cloud.formal.dataflow.{DataflowGraph, InfrastructureGraph}
import com.cloud.formal.reasoning.PropertiesChecker
import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.cli.{DefaultParser, HelpFormatter, Option, Options, ParseException}


object CLI  extends LazyLogging
{

  private val isPrintEnabled: Boolean = true

  def run(args: Array[String]): Unit =
  {

    getCommand(options(),args) match
    {
      case None => exitWithMessage(" Exiting")
      case Some(c) =>
        if (c.getOptions.isEmpty)
        {
          println("\nNo options selected, please see help:\n")
          help()
        }
        c.iterator().forEachRemaining(
          o => o.getOpt match
          {
            case "s"  => spec(o)
            case "m"  => model(o)
            case "ma" => modelAll(o)
            case "r"  => reason(o)
            case "mr" => modelAndReason(o)
            case "ra" => reasonAll(o)
            case "b"  => benchmark()
            case "ig" => infrastructureGraph()
            case "d"  => dataflowGraph()
            case "h"  => help()
            case  _   => println("Unknown Option Selected")
          }
        )
    }

  }





  private def options(): Options =
  {

    val spec = new Option("s","spec",true,
      "Compiles the CloudFormation Resource Specification " +
        "folder (passed as 1st arg) to .owl Terminologies (saved " +
        "in the folder passed as 2nd argument)")
    spec.setArgs(2)
    spec.setOptionalArg(true)
    spec.setArgName("PATH")

    val model = new Option("m","mod",true,
      "Compiles the CloudFormation templates " +
        "folder (passed as 1st arg) and saves the Infrastructure Model" +
        " in output dir (passed as 2nd arg). If no output dir is given," +
        " default is \"BenchmarksOut/\".")
    model.setArgs(2)
    model.setOptionalArg(true)
    model.setArgName("PATH")

    val modelAll = new Option("ma","modAll",true,
      "Compiles ALL CloudFormation templates in all " +
        "folders under the input one (passed as 1st arg) and saves their Infrastructure Models in " +
        "output dir (passed as 2nd " +
        "arg). If no output dir is given, default is \"BenchmarksOut/\".")
    modelAll.setArgs(2)
    modelAll.setOptionalArg(true)
    modelAll.setArgName("PATH")

    val modelAndReason = new Option("mr", "modReas", true,
    "Compiles CloudFormation templates in input folder" +
      " (passed as 1st arg), saves them in output folder (passed as" +
      " 2nd arg), and runs the checks - saving a *Report.csv in the" +
      " output folder")
    modelAndReason.setArgs(2)
    modelAndReason.setOptionalArg(true)
    modelAndReason.setArgName("PATH")

    val reasoningAll = new Option("ra", "reasAll", true,
    "Runs the checks in all folders containing Infrastructure" +
      " Models under the input folder (passed as 1st arg). Writes *Report.csv" +
      " in the same corresponding folders.")
    reasoningAll.setArgName("PATH")

    val reasoning = new Option("r","reas", true,
      "Runs the checks on the Infrastructure Model in input " +
        "(passed as 1st arg). Writes *Report.csv in the same folder.")
    reasoning.setArgName("PATH")

    val benchmark = new Option("b", "bench", false,
    "Runs benchmarks to generate Sec. 6's table.")

    val infrGraph = new Option("ig", "infrG", false,
    "Generates a graph representation of the sample " +
      "infrastructure from Sec. 7.")

    val dfdGraph = new Option("d","dfd", false,
    "Extends the sample infrastructure of Sec. 7 with dataflow" +
    " knowledge and generates a graph of the resulting, inferred, dataflow diagram.")

    val help = new Option("h","help",false, "")

    new Options()
      .addOption(spec)
      .addOption(model)
      .addOption(modelAll)
      .addOption(modelAndReason)
      .addOption(reasoning)
      .addOption(reasoningAll)
      .addOption(benchmark)
      .addOption(infrGraph)
      .addOption(dfdGraph)
      .addOption(help)

  }


  private def getCommand(opts: Options, args: Array[String])=
  {
    try
      Some(new DefaultParser().parse(opts, args))
    catch {
      case _:ParseException
      => print("Unable to parse command.")
        None
    }
  }

  private def help(): Unit = {
    val header = "\nArguments:"
    val footer = "\nExamples: \n -s  src/main/resources/CloudFormationResourceSpecification" +
      "\n -m  Benchmarks/16_retailmenot/ ~/Outputs/" +
      "\n -r  ~/Outputs/16_retailmenot/16_retailmenot_InfrastructureModel.owl" +
      "\n -ma Benchmarks/ ~/Outputs/" +
      "\n -mr Benchmarks/16_retailmenot/" +
      "\n -ra BenchmarksOut/" +
      "\n "
    val formatter = new HelpFormatter
    formatter.printHelp("CloudFORMAL",
      header, options(), footer, true)
  }


  private def exitWithMessage(msg: String): Unit = {
    println(msg)
    System.exit(0)
  }


  private def queryModel(dir: File): Unit =
  {
    val t =
      Interface.loadModel(
        dir.listFiles().filter(_.getName.endsWith(Extension.Owl))
          .head.getAbsolutePath)
    val pc = new PropertiesChecker(t._4,t._1,t._2,t._3,dir.getAbsolutePath)
    pc.run(isPrintEnabled)(pc.r.runQuery)(pc.r.isSat)
  }

  private def spec(o: Option): Unit =
  {
    val inPath  = o.getValue(0).replace("~",System.getProperty(SysUtil.UserHome))
    val outPath =
      if (o.getValues.length == 2)
        o.getValue(1).replace("~", System.getProperty(SysUtil.UserHome))
      else FilePath.ResourceTerms
    Interface compileAndSaveSpecification(inPath, outPath, isPrintEnabled)
  }

  private def model(o: Option) =
  {
    val inPath  = o.getValue(0).replace("~",System.getProperty(SysUtil.UserHome))
    val outPath =
      if (o.getValues.length == 2)
        o.getValue(1).replace("~", System.getProperty(SysUtil.UserHome))
      else FilePath.BenchmarksOut
    Interface.modelAndSaveTemplates(inPath, outPath, Interface.createInfrastructure(isPrintEnabled))
  }

  private def modelAll(o: Option) =
  {
    val inPath  = o.getValue(0).replace("~",System.getProperty(SysUtil.UserHome))
    val outPath =
      if (o.getValues.length == 2)
        o.getValue(1).replace("~", System.getProperty(SysUtil.UserHome))
      else FilePath.BenchmarksOut
    Interface.modelAndSaveAllTemplates(inPath, outPath, Interface.createInfrastructure(isPrintEnabled))
  }

  private def reason(o: Option): Unit =
  {
    val dir = o.getValue.split("/").toVector.dropRight(1).mkString("/")
    val t = Interface.loadModel(o.getValue())
    val pc = new PropertiesChecker(t._4,t._1,t._2,t._3,dir)
    pc.run(isPrintEnabled)(pc.r.runQuery)(pc.r.isSat)
  }

  private def modelAndReason(o: Option): Unit =
  {
    val dir = FilePath.BenchmarksOut + "/" + o.getValue().split("/").last
    val m = Interface.compileAndSaveTemplates(o.getValue, Interface.createInfrastructure(isPrintEnabled))
    val pc = new PropertiesChecker(m._1,m._2,m._3,m._4,dir)
    pc.run(isPrintEnabled)(pc.r.runQuery)(pc.r.isSat)
  }

  private def reasonAll(o: Option): Unit =
  {
    new File(o.getValue.replace("~",System.getProperty(SysUtil.UserHome)))
      .listFiles().filter(_.isDirectory).sortBy(_.getName)
      .foreach( dir => queryModel(dir) )
  }

  private def benchmark(): Unit =
  {
    BenchmarkRunner.run()
  }

  private def infrastructureGraph(): Unit =
  {
    InfrastructureGraph.run()
  }

  private def dataflowGraph(): Unit =
  {
    DataflowGraph.run()
  }


}
