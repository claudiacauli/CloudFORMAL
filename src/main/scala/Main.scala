
import java.io.{File, FileNotFoundException}

import argonaut.Json
import aws.cfn.dlmodel.DLModelWriter
import aws.cfn.dlmodel.template.StackSetModel
import aws.cfn.shared.ParseUtils
import aws.cfn.specifications.encoding.{Json2SpecificationEncoder, Map2ToServiceActions, ServiceActions2DLEncoder, Specification2DLEncoder}
import aws.cfn.specifications.formalization.StringProperty
import aws.cfn.templates.encoding.{Infrastructure2DLEncoder, Json2InfrastructureEncoder, Json2StackSetEncoder, StackSet2DLEncoder}
import aws.cfn.templates.formalization._

import scala.language.postfixOps
import scala.jdk.CollectionConverters._

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

  //recompileTerminology()
  modelZelkovaTest()


  def modelZelkovaTest() = {

    val inputFilePath = "src/main/resources/InputStackSets/Zelkova/test/"
    val outputFilePath = "/Users/caulic/IdeaProjects/CloudLogic/src/main/resources/OutputModels/ZelkovaTest/"
    val inputDir = new File(inputFilePath)
    (inputDir.listFiles() filter (f => f.isDirectory)) foreach( f => createInfrastructure(f) )

    def createInfrastructure(file: File) = {

      val infrastructureName = file.getName

      val i = Json2InfrastructureEncoder.encode(
        ((file.listFiles() filter (f => f.isDirectory)) map ( f => createStackSetFiles(f,infrastructureName) )).toVector,
        infrastructureName)
      val iM = Infrastructure2DLEncoder.encode(i)

      iM.writeToOutputFolder(outputFilePath)

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

          val tmplJson = ParseUtils.jsonFromFilePath(f.getAbsolutePath).get
          val descrJson = ParseUtils.jsonFromFilePath(descriptor.getAbsolutePath)
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


 def recompileTerminology() = {
     printOntologiesFromResourceSpecificationDirectoryToFolder(
       "/Users/caulic/Downloads/CloudFormationResourceSpecification/",
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


     def getOntologyFromResourceSpecificationFilePath(filepath: String) =
       getOntologyFromResourceSpecificationFile(new File(filepath))


     def getOntologyFromResourceSpecificationFile(file: File) =
       Specification2DLEncoder.encode(
         Json2SpecificationEncoder.encode(
           ParseUtils.jsonFromFile(file).get,
           file.getName.split("/").last.split("Specification.json").head))





     def printOntologyFromResourceSpecificationFilePathToFolder(filePath: String, outputPath: String): Unit =
       printOntologyFromResourceSpecificationFileToFolder(new File(filePath), outputPath)



     def printOntologyFromResourceSpecificationFileToFolder(file : File, outputPath: String): Unit = {
       println("[TBox]  " + file.getName.split("Specification.json")(0))
       getOntologyFromResourceSpecificationFile(file).writeToOutputFolder(outputPath)
     }
 }



//  val tmplJson = Parser.jsonFromFilePath("src/main/resources/InputStackSets/BucketWithLogging/s3bucket_with_logging_bucket.json").get
//  val descrJson = Parser.jsonFromFilePath("src/main/resources/InputStackSets/BucketWithLogging/descriptor.json")
//  val ss = Json2StackSetEncoder.encode(Vector(("testBucketLogging",tmplJson,descrJson)),"testBucketLogging")
//  val ssM = StackSet2DLEncoder.encode(ss)
//  OntologyWriter.writeStackSetToOutputFolder(ssM, "src/main/resources/OutputModels/" )










//

  /*
    ACTION FUNCTIONALITIES!
  */
//  def updateActionsOntologiesInProjectResources(): Unit =
//    saveActionsOntologyInFolder("src/main/resources/terminology/actions/")
//
//
//  def saveActionsOntologyInFolder(folderPath : String): Unit =
//    ServiceActions2DLEncoder.encode(Map2ToServiceActions.fromMap())  foreach ( aM => aM.writeToOutputFolder("src/main/resources/terminology/actions/") )


}
