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

import com.cloud.formal.{Extension, SysUtil}
import com.cloud.formal.binding.Interface
import com.cloud.formal.reasoning.PropertiesChecker
import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.cli.{DefaultParser, HelpFormatter, Option, Options, ParseException}


object CLI  extends LazyLogging{

  def run(args: Array[String]): Unit = {

    getCommand(options(),args) match {
      case None     =>
        exitWithMessage(" Exiting.")
      case Some(c)  =>
        if (c.getOptions.isEmpty)
          exitWithMessage("Null or invalid arguments.")
        c.iterator().forEachRemaining(
          o => o.getOpt match {
            case "s" => Interface.compileAndSaveSpecification(o.getValue())
            case "m" => Interface.modelAndSaveTemplates(o.getValues, Interface.createInfrastructure)
            case "ma" => Interface.modelAndSaveAllTemplates(o.getValues, Interface.createInfrastructure)
            case "r" => val t = Interface.loadModel(o.getValue())
              val pc = new PropertiesChecker(t._4,t._1,t._2,t._3)
              pc.run(true)(pc.r.runQuery)(pc.r.isSat)
            case "mr" =>
              val m = Interface.compileAndSaveTemplates(o.getValue, Interface.createInfrastructure)
              val pc = new PropertiesChecker(m._1,m._2,m._3,m._4)
              pc.run(true)(pc.r.runQuery)(pc.r.isSat)
            case "ra" =>
              new File(o.getValue.replace("~",System.getProperty(SysUtil.UserHome)))
                .listFiles()
                .filter(_.isDirectory)
                .sortBy(_.getName)
                .foreach( dir => {
                 val f = dir.listFiles().filter(_.getName.endsWith(Extension.Owl)).head
                  val t = Interface.loadModel(f.getAbsolutePath)
                  val pc = new PropertiesChecker(t._4,t._1,t._2,t._3,dir.getAbsolutePath)
                  pc.run(true)(pc.r.runQuery)(pc.r.isSat)
                })
            case "h" => help()
            case  _  => println("Unknown Option Selected")
          }
        )
    }

  }






  private def options(): Options = {

    val spec = new Option("s","spec",true,
      "Compile and save .owl terminology from " +
        "the CloudFormation resource specifications folder" +
        " passed as argument.")
    spec.setArgName("PATH")

    val model = new Option("m","model",true,
      "Generate and save .owl model from the CloudFormation templates " +
        "folder passed as first argument and saves them in the dir passed as second " +
        "argument. If no output dire is given, default is \"BenchmarksOut/\".")
    model.setArgName("PATH")

    val modelAll = new Option("ma","modelAll",true,
      "Generate and save .owl models from ALL the CloudFormation templates " +
        "folders contained in the folder passed as first argument and saves them in " +
        "the dir passed as second " +
        "argument. If no output dire is given, default is \"BenchmarksOut/\".")
    model.setArgName("PATH")

    val modelAndReason = new Option("mr", "modelAndReason", true,
    "Generates and save .owl model from the CloudFormation template folder" +
      " passed as first argument and saves them in the folder passed as second argoment." +
      " Also, run the queries for the just generated model.")

    val reasoningAll = new Option("ra", "reasonAll", true,
    "Run queries on the generated .owl models and outputs a report, for ALL" +
      " infrastructure folders contained in the folder passed as argument.")
    model.setArgName("PATH")

    val reasoning = new Option("r","reason", true,
      "Run queries on the generated .owl model and outputs a report.")
    model.setArgName("PATH")

    val help = new Option("h","help",false, "")

    new Options()
      .addOption(spec)
      .addOption(model)
      .addOption(modelAll)
      .addOption(modelAndReason)
      .addOption(reasoning)
      .addOption(reasoningAll)
      .addOption(help)

  }


  private def getCommand(opts: Options, args: Array[String]) =
    try
      Some(new DefaultParser().parse(opts,args))
    catch {
      case e:ParseException
      => print("Unable to parse command.")
        None
    }


  private def help() = {
    val header = "Arguments:"
    val footer = ""
    val formatter = new HelpFormatter
    formatter.printHelp("CloudFORMAL",
      header, options(), footer, true)
  }


  private def exitWithMessage(msg: String): Unit = {
    println(msg)
    System.exit(0)
  }



}
