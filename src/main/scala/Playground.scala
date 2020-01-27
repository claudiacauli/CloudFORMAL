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

import java.io.{File, FileNotFoundException}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import argonaut.{Json, Parse}
import com.cloud.formal.mapping.specifications.ResourceSpecificationModel
import com.cloud.formal.mapping.templates.Json2InfrastructureEncoder
import com.cloud.formal.mapping.templates.mapping.InfrastructureModel
import com.cloud.formal.model.ProtegeCatalogue

import scala.io.Source
import scala.language.postfixOps

object Playground extends App {

  // recompileTerminologyInProjectResources()
  // modelSampleInputs()

  modelBenchmarks()

  private def modelBenchmarks(): Unit = {

    val inPath = "Benchmarks"

    val outPath = "BenchmarksOut/"

    new File(inPath)
      .listFiles()
      .filter(_.isDirectory)
      .foreach(iF => modelInputDir(iF.getAbsolutePath(),outPath))
  }


//
//  modelInputDir("src/main/resources/InputStackSets/CaseStudy0_T/"
//    ,"src/main/resources/OutputModels/")
//
//  modelInputDir("src/main/resources/InputStackSets/CaseStudy1_Z/"
//    ,"src/main/resources/OutputModels/")
//
//
  private def modelSampleInputs(): Unit =
  {
    val inputFilePaths = Vector(
      "src/main/resources/SampleInputs/Infrastructure1_BucketAnalyticsInventory/",
      "src/main/resources/SampleInputs/Infrastructure2_BucketCloudFront/",
      "src/main/resources/SampleInputs/Infrastructure3_BucketCors/",
      "src/main/resources/SampleInputs/Infrastructure4_BucketDefaultEncryption/",
      "src/main/resources/SampleInputs/Infrastructure5_BucketLambdaConfig/",
      "src/main/resources/SampleInputs/Infrastructure6_BucketRetainOnDelete/",
      "src/main/resources/SampleInputs/Infrastructure7_cfn-modules-s3/",
      "src/main/resources/SampleInputs/Infrastructure8_cfn-modules-s3-kms/",
      "src/main/resources/SampleInputs/Infrastructure9_cfn-modules-s3-kms-lambda/"
    )
    val outputFilePath = "src/main/resources/SampleOutputs/"

    inputFilePaths.foreach(iF => modelInputDir(iF,outputFilePath))
  }



  private def modelInputDir(inputFilePath: String, outputFilePath: String) = {

    println("Modeling " + inputFilePath)

    def createInfrastructure(file: File): Unit = {

      val infrastructureName = file.getName

      val i = Json2InfrastructureEncoder.encode(
        file.listFiles()
          .filter(_.isDirectory)
          .map ( f =>
            createStackSetFiles(f,infrastructureName)).toVector,
        infrastructureName)

      val infrastructureModel =
        InfrastructureModel.fromInfrastructure(i)

      infrastructureModel.writeToOutputFolder(outputFilePath)
      i.writeInfrastructureSummaryToFolder(outputFilePath)

    }


    def createStackSetFiles(file: File, iN:String):
    (Vector[(String, Json, Option[Json])], String) =
    {
      val stackSetName = file.getName

      (file.listFiles().toVector
          .filter(_.getAbsolutePath.endsWith(".json"))
        flatMap (f => {
        val templateName = f.getName.split(".json").head
        if (!templateName
          .endsWith("Descriptor") && !templateName.endsWith("DS_Store"))
        {
          var descriptor: File = null
          try {
            descriptor = new File(inputFilePath + "/" + iN +"/" +stackSetName + "/" + templateName + "Descriptor.json")
            if (!descriptor.exists)
              createDescriptorForTemplateName(templateName, file.getAbsolutePath())
          } catch {
            case e: FileNotFoundException => descriptor = null
          }

          val tmplJson  = Parse.parseOption(Source.fromFile(f).mkString).get
          val descrJson = Parse.parseOption(Source.fromFile(descriptor).mkString)
          Vector((templateName, tmplJson, descrJson))
        } else Vector()
      }),stackSetName )
    }


    new File(inputFilePath)
      .listFiles()
      .filter(_.isDirectory)
      .foreach(createInfrastructure)
  }



  private def createDescriptorForTemplateName(templateName: String, inputDir: String) :Unit = {
    val ds = "{\n  " +
      "\"AWS::Region\" : \"us-east-1\",\n  " +
      "\"AWS::Partition\" : \"aws\",\n  " +
      "\"AWS::StackId\" : \"121213\",\n  " +
      "\"AWS::AccountId\" : \"999999999999\",\n  " +
      "\"AWS::NotificationARNs\" : \"arn:aws:cnc:sjkjs:as\",\n  " +
      "\"AWS::StackName\" : \"" + templateName + "\",\n  " +
      "\"AWS::URLSuffix\" : \".aws\"\n}"

    Files.write(
      Paths.get(inputDir + "/" + templateName + "Descriptor.json"),
      ds.getBytes(StandardCharsets.UTF_8)
    )
  }




  private def recompileTerminologyInProjectResources(): Unit = {
    printOntologiesFromResourceSpecificationDirectoryToFolder(
      System.getProperty("user.home") + "/Downloads/CloudFormationResourceSpecification/",
      "src/main/resources/terminology/resourcespecificationsOwl/"
    )



    def printOntologiesFromResourceSpecificationDirectoryToFolder(dirPath: String, outputPath : String): Unit = {
      val dir = new File(dirPath)
      if ( !dir.isDirectory ) {
        throw new FileNotFoundException( "The directory " + dirPath + " does not exist")
      }
      else
        dir.listFiles(f => f.getName.endsWith("Specification.json") && !f.getName.equals("CloudFormationResourceSpecification.json"))
          .foreach ( f => printOntologyFromResourceSpecificationFileToFolder(f,outputPath) )
    }



    def getOntologyFromResourceSpecificationFile(file: File) =
      ResourceSpecificationModel.fromResourceSpecificationFile(file)



    def printOntologyFromResourceSpecificationFileToFolder(file : File, outputPath: String): Unit = {
      println("[TBox]  " + file.getName.split("Specification.json")(0))
      getOntologyFromResourceSpecificationFile(file).writeToOutputFolder(outputPath)
    }
  }






}