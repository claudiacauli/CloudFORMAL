
import java.io.{File, FileNotFoundException}

import argonaut.{Json, Parse}
import com.cloud.formal.mapping.actions.{ActionsMap, ActionsModel}
import com.cloud.formal.mapping.specifications.ResourceSpecificationModel
import com.cloud.formal.mapping.templates.Json2InfrastructureEncoder
import com.cloud.formal.mapping.templates.mapping.{InfrastructureModel, PermissionsModel}

import scala.io.Source
import scala.language.postfixOps

object Playground extends App {

  // recompileTerminologyInProjectResources()
  // updateActionsOntologiesInProjectResources()
  modelSampleInputs()


  private def modelSampleInputs(): Unit =
  {
    val inputFilePaths = Vector(
      "src/main/resources/SampleInputs/Infrastructure1_BucketAnalyticsInventory/",
      "src/main/resources/SampleInputs/Infrastructure2_BucketCloudFront/",
      "src/main/resources/SampleInputs/Infrastructure3_BucketCors/",
      "src/main/resources/SampleInputs/Infrastructure4_BucketDefaultEncryption/",
      "src/main/resources/SampleInputs/Infrastructure5_BucketLambdaConfig/",
      "src/main/resources/SampleInputs/Infrastructure6_BucketRetainOnDelete/"
    )
    val outputFilePath = "src/main/resources/SampleOutputs/"

    inputFilePaths.foreach(iF => modelInputDir(iF,outputFilePath))
  }



  private def modelInputDir(inputFilePath: String, outputFilePath: String) = {

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
      val permissionModel =
        PermissionsModel.fromInfrastructure(i)

      infrastructureModel.writeToOutputFolder(outputFilePath)
      i.writeInfrastructureSummaryToFolder(outputFilePath)
      i.writePolicySummaryToFolder(outputFilePath)
      permissionModel.writeToOutputFolder(outputFilePath)

    }


    def createStackSetFiles(file: File, iN:String):
    (Vector[(String, Json, Option[Json])], String) =
    {
      val stackSetName = file.getName

      (file.listFiles().toVector flatMap (f => {
        val templateName = f.getName.split(".json").head
        if (!templateName
          .endsWith("Descriptor") && !templateName.endsWith("DS_Store"))
        {
          var descriptor: File = null
          try {
            descriptor = new File(inputFilePath + iN +"/" +stackSetName + "/" + templateName + "Descriptor.json")
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


  private def updateActionsOntologiesInProjectResources(): Unit =
    saveActionsOntologyInFolder("src/main/resources/terminology/actions/")

  private def saveActionsOntologyInFolder(folderPath : String): Unit =
    ActionsMap.getActionPrefixes
      .flatMap(s => ActionsModel.getFromServiceName(s))
      .foreach(_.writeToOutputFolder(folderPath))





}