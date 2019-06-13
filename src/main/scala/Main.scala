
import java.io.{File, FileNotFoundException}

import argonaut.{Json, Parse}
import aws.cfn.actions.{ActionsMap, ActionsMapper, ActionsModel, ActionsModelMapper}
import aws.cfn.specifications.ResourceSpecificationModel
import aws.cfn.templates.{InfrastructureModel, PermissionsModel}
import aws.cfn.templates.Json2InfrastructureEncoder

import scala.io.Source
import scala.language.postfixOps

object Main extends App {


  /*
  TODO!!!
  Implement this chain of functions to have the stackSet store a map of resources by account!
  Going through all templates essentially :)
   */

//    val t1 = ("Acc1", Vector(1,2,3,4))
//    val t2 = ("Acc2", Vector(5,6,7,8))
//    val t3 = ("Acc2", Vector(9))
//    val t4 = ("Acc1", Vector(10,11))
//    val vt = Vector(t1,t2,t3,t4)
//
//    println(vt.groupBy(e => e._1).toVector.flatMap( e => Map(e._1 -> e._2.flatMap( p => p._2 ) )))

  recompileTerminology()
  updateActionsOntologiesInProjectResources()
  modelZelkovaTest()


  def modelZelkovaTest(): Unit = {

    val inputFilePath = "src/main/resources/InputStackSets/Zelkova/test/"
    val outputFilePath = "/Users/claudia/IdeaProjects/CloudLogic/src/main/resources/OutputModels/ZelkovaTest/"
    val inputDir = new File(inputFilePath)
    (inputDir.listFiles() filter (f => f.isDirectory)) foreach( f => createInfrastructure(f) )

    def createInfrastructure(file: File): Unit = {

      val infrastructureName = file.getName

      val i = Json2InfrastructureEncoder.encode(
        ((file.listFiles() filter (f => f.isDirectory)) map ( f => createStackSetFiles(f,infrastructureName) )).toVector,
        infrastructureName)

      val infrastructureModel = InfrastructureModel.fromInfrastructure(i)
      val permissionModel = PermissionsModel.fromInfrastructure(i)

      infrastructureModel.writeToOutputFolder(outputFilePath)
      i.writeInfrastructureSummaryToFolder(outputFilePath)
      i.writePolicySummaryToFolder(outputFilePath)
      permissionModel.writeToOutputFolder(outputFilePath)

    }



    def createStackSetFiles(file: File, infrastructureName:String): (Vector[(String, Json, Option[Json])], String) = {
      val stackSetName = file.getName

      (file.listFiles().toVector flatMap (f => {
        val templateName = f.getName.split(".json").head
        if (!templateName.endsWith("Descriptor") && !templateName.endsWith("DS_Store")) {

          var descriptor: File = null
          try {
            descriptor = new File(inputFilePath + infrastructureName +"/" +stackSetName + "/" + templateName + "Descriptor.json")
          } catch {
            case e: FileNotFoundException => descriptor = null
          }



          val tmplJson  = Parse.parseOption(Source.fromFile(f).mkString).get
          val descrJson = Parse.parseOption(Source.fromFile(descriptor).mkString)
          Vector((templateName, tmplJson, descrJson))
        } else Vector()
      }),stackSetName )
    }



      //ssM.writeToOutputFolder("/Users/caulic/IdeaProjects/CloudLogic/src/main/resources/OutputModels/ZelkovaTest/" )
      //ssM.pruneToDataFlowModel().writeToOutputFolder("/Users/caulic/IdeaProjects/CloudLogic/src/main/resources/OutputModels/ZelkovaTest/")

      //
      ////    file.listFiles().toVector foreach ( f => {
      ////      if(!f.getName.equals("descriptor.json")) {
      ////        val tmplJson = Parser.jsonFromFilePath( f.getAbsolutePath ).get
      ////        val descrJson = Parser.jsonFromFilePath( descriptor.getAbsolutePath )
      ////        val ss = Json2StackSetEncoder.encode(Vector((f.getName,tmplJson,descrJson)),stackSetName)
      ////        val ssM = StackSet2DLEncoder.encode(ss)
      ////        OntologyWriter.writeStackSetToOutputFolder(ssM, "src/main/resources/OutputModels/ZelkovaTest/" )
      ////      }
      ////    })
      //
    //}
  }


 def recompileTerminology(): Unit = {
     printOntologiesFromResourceSpecificationDirectoryToFolder(
       "/Users/claudia/Downloads/CloudFormationResourceSpecification/",
       "src/main/resources/terminology/resourcespecificationsOwl/"
     )



     def printOntologiesFromResourceSpecificationDirectoryToFolder(dirPath: String, outputPath : String): Unit = {
       val dir = new File(dirPath)
       if ( !dir.isDirectory )
         throw new FileNotFoundException( "The directory " + dirPath + " does not exist")
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



  /*
    ACTION FUNCTIONALITIES!
  */

  def updateActionsOntologiesInProjectResources(): Unit =
    saveActionsOntologyInFolder("src/main/resources/terminology/actions/")

  def saveActionsOntologyInFolder(folderPath : String): Unit =
    ActionsMap.getActionPrefixes
      .flatMap(s => ActionsModel.getFromServiceName(s))
          .foreach(_.writeToOutputFolder(folderPath))

}
