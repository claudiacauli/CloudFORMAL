import java.io.{File, FileNotFoundException}

import aws.cfn.dlmodel.OntologyWriter
import aws.cfn.encoding.Parser
import aws.cfn.encoding.specification.JsonSpecificationEncoder
import aws.cfn.encoding.specification.SpecificationDLEncoder



object Main extends App {


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


  def getOntologyFromResourceSpecificationFilePath(filepath: String) =
    getOntologyFromResourceSpecificationFile(new File(filepath))


  def getOntologyFromResourceSpecificationFile(file: File) =
    SpecificationDLEncoder.encode(
      JsonSpecificationEncoder.encode(
        Parser.jsonFromFile(file).get,
        file.getName.split("/").last.split("Specification.json").head))





  def printOntologyFromResourceSpecificationFilePathToFolder(filePath: String, outputPath: String): Unit =
    printOntologyFromResourceSpecificationFileToFolder(new File(filePath), outputPath)



  def printOntologyFromResourceSpecificationFileToFolder(file : File, outputPath: String): Unit = {
    println("[TBox]  " + file.getName.split("Specification.json")(0))
    OntologyWriter.writeToOutputDir(getOntologyFromResourceSpecificationFile(file), outputPath)
  }




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
