
import java.io.{File, FileNotFoundException}

import aws.cfn.dlmodel.OntologyWriter
import aws.cfn.encoding.Parser
import aws.cfn.encoding.template.{Json2StackSetEncoder, StackSet2DLEncoder}



object Main extends App {

  val inputDir = new File("src/main/resources/InputStackSets/")
  (inputDir.listFiles() filter (f => f.isDirectory)) foreach( f => createStackSet(f) )

  def createStackSet(file: File) = {
    val stackSetName = file.getName
    var descriptor : File = null

    try {
      descriptor = new File("src/main/resources/InputStackSets/" + stackSetName +"/descriptor.json")
    } catch {
      case e: FileNotFoundException => descriptor = null
    }

    println("Checking subfiles in directory: " + file)

    file.listFiles().toVector foreach ( f => {
      if(!f.getName.equals("descriptor.json")) {
        val tmplJson = Parser.jsonFromFilePath( f.getAbsolutePath ).get
        val descrJson = Parser.jsonFromFilePath( descriptor.getAbsolutePath )
        val ss = Json2StackSetEncoder.encode(Vector((f.getName,tmplJson,descrJson)),stackSetName)
        val ssM = StackSet2DLEncoder.encode(ss)
        OntologyWriter.writeToOutputDir(ssM, "src/main/resources/OutputModels/" )
      }
    })

  }

//  val tmplJson = Parser.jsonFromFilePath("src/main/resources/InputStackSets/BucketWithReplicaAndIamRole/s3bucket_with_replica_and_IAMrole.json").get
//  val descrJson = Parser.jsonFromFilePath("src/main/resources/InputStackSets/BucketWithReplicaAndIamRole/descriptor.json")
//  val ss = Json2StackSetEncoder.encode(Vector(("bucketWithLogging",tmplJson,descrJson)),"BucketWithReplicaAndIam")
//  val ssM = StackSet2DLEncoder.encode(ss)
//  OntologyWriter.writeToOutputDir(ssM, "src/main/resources/OutputModels/" )

//  printOntologiesFromResourceSpecificationDirectoryToFolder(
//    "/Users/claudia/Downloads/CloudFormationResourceSpecification/",
//    "src/main/resources/terminology/resourcespecificationsOwl/"
//  )
//
//
//  def printOntologiesFromResourceSpecificationDirectoryToFolder(dirPath: String, outputPath : String): Unit = {
//    val dir = new File(dirPath)
//    if ( !dir.isDirectory )
//      throw new FileNotFoundException( "The directory " + dirPath + " does not exist")
//    else
//      dir.listFiles(f => f.getName.endsWith("Specification.json") && !f.getName.equals("CloudFormationResourceSpecification.json"))
//        .foreach ( f => printOntologyFromResourceSpecificationFileToFolder(f,outputPath) )
//  }
//
//
//  def getOntologyFromResourceSpecificationFilePath(filepath: String) =
//    getOntologyFromResourceSpecificationFile(new File(filepath))
//
//
//  def getOntologyFromResourceSpecificationFile(file: File) =
//    SpecificationDLEncoder.encode(
//      JsonSpecificationEncoder.encode(
//        Parser.jsonFromFile(file).get,
//        file.getName.split("/").last.split("Specification.json").head))
//
//
//
//
//
//  def printOntologyFromResourceSpecificationFilePathToFolder(filePath: String, outputPath: String): Unit =
//    printOntologyFromResourceSpecificationFileToFolder(new File(filePath), outputPath)
//
//
//
//  def printOntologyFromResourceSpecificationFileToFolder(file : File, outputPath: String): Unit = {
//    println("[TBox]  " + file.getName.split("Specification.json")(0))
//    OntologyWriter.writeToOutputDir(getOntologyFromResourceSpecificationFile(file), outputPath)
//  }




////
//
//  /*
//    ACTION FUNCTIONALITIES!
//  */
//  def updateActionsOntologiesInProjectResources(): Unit =
//    saveActionsOntologyInFolder("src/main/resources/terminology/actions/")
//
//
//  def saveActionsOntologyInFolder(folderPath : String): Unit =
//    ServiceActionsGenerator.fromMap() foreach (sa =>
//      OntologyWriter.write(new ServiceActionsMapper(sa).map(), folderPath))
//


}
