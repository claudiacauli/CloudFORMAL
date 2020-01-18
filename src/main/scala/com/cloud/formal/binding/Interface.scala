package com.cloud.formal.binding

import java.io.{File, FileNotFoundException}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util

import argonaut.{Json, Parse}
import com.cloud.formal.{Benchmarking, FilePath}
import com.cloud.formal.mapping.specifications.ResourceSpecificationModel
import com.cloud.formal.mapping.templates.mapping.InfrastructureModel
import com.cloud.formal.mapping.templates.{Infrastructure, Json2InfrastructureEncoder}
import com.cloud.formal.model.{Model, ModelIRI}
import com.typesafe.scalalogging.LazyLogging
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.model.parameters.Imports

import Console.{BOLD, RESET}
import scala.io.Source



object Interface extends LazyLogging{




  def compileAndSaveSpecification(inPath: String): Unit = {

    val inputPath = inPath.replace("~",System.getProperty("user.home"))
    val dir = new File(inputPath)

    if ( !dir.exists || !dir.isDirectory )
      exitWithMessage("Argument does not point " +
        "to an existing and valid directory.")

    val specModels =
      dir.listFiles().toVector
        .filter(!_.getName.startsWith("CloudFormationResource"))
        .map(f =>
          ResourceSpecificationModel
            .fromResourceSpecificationFile(f))

    specModels.foreach( sm => {
      println(s" [Spec-OWL] ${sm.name}")
      val dir = new File(FilePath.ResourceTerms)
      if (!dir.exists())
        dir.mkdir()
      sm.writeToOutputFolder(FilePath.ResourceTerms)
    })

  }




  def compileAndSaveTemplates(inPath: String, outputPath: String = "BenchmarksOut/") = {

    val inputPath = inPath.replace("~",System.getProperty("user.home"))


    def createInfrastructure(file: File) = {
      val infrastructureName = file.getName

      var t = System.nanoTime()
      var i : Infrastructure = null
      var im : Model = null

      println("\n "+ infrastructureName)

      //  val p = Benchmarking.timeN(100)("Encoding", encodeInfrastructure(file,infrastructureName))
      val p = encodeInfrastructure(file,infrastructureName)
      i = p._1
      im = p._2

      println(s" - [Encoding] Resources count: ${i.getResourcesCount}")
      println(s" - [Encoding] Resource types count: ${i.getResourceTypesCount}")

      im.writeToOutputFolder(outputPath)
      i.writeInfrastructureSummaryToFolder(outputPath)
      (im.name, im.ontology, im.df, im.manager)
    }


    def encodeInfrastructure(file: File, infrastructureName: String) = {

      val i =
        Json2InfrastructureEncoder.encode(
          file.listFiles()
            .filter(_.isDirectory)
            .map ( f =>
              createStackSetFiles(f,infrastructureName)).toVector,
          infrastructureName)

      val im =
        InfrastructureModel.fromInfrastructure(i)

      (i,im)
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
            descriptor = new File(inputPath + "/" + iN +"/" +stackSetName + "/" + templateName + "Descriptor.json")
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


    new File(inputPath)
      .listFiles()
      .filter(_.isDirectory)
      .map(createInfrastructure)
      .head

  }



  def modelAndSaveAllTemplates(values: Array[String]): Unit = {

    val inputPath = values(0).replace("~",System.getProperty("user.home"))

    val outputPath =
      if (values.size == 2)
        values(1).replace("~", System.getProperty("user.home"))
      else
        "BenchmarksOut/"

    new File(inputPath)
      .listFiles()
      .filter(_.isDirectory)
      .foreach(iF => modelAndSaveTemplates(Array(iF.getAbsolutePath,outputPath)))
  }


  def modelAndSaveTemplates(values: Array[String]): Unit = {

    val inputPath = values(0).replace("~",System.getProperty("user.home"))

    val outputPath =
      if (values.size == 2)
        values(1).replace("~", System.getProperty("user.home"))
      else
        "BenchmarksOut/"

     compileAndSaveTemplates(inputPath, outputPath)
  }



  def loadModel(inPath: String): (OWLOntology, OWLDataFactory, OWLOntologyManager, String) = {
    val inputPath = inPath.replace("~",System.getProperty("user.home"))
    val m = OWLManager.createOWLOntologyManager()
    val df = m.getOWLDataFactory

    val name = inputPath.split("_InfrastructureModel.owl")(0).split("/").last

    val preDir = new File( inPath.split("/").dropRight(1).mkString("/") )

    print(s"\n******************************************" +
      s"*******************************************")

    preDir.listFiles().filter(_.isDirectory)
      .foreach(_.listFiles().filter(f => f.getName.endsWith(".owl") && !f.getName.endsWith("StackSetModel.owl"))
        .foreach( f => {
          m.loadOntologyFromOntologyDocument(f)
        }))

    preDir.listFiles().filter(_.isDirectory)
      .foreach(_.listFiles().filter(_.getName.endsWith("StackSetModel.owl"))
        .foreach( f => {
          val o = m.loadOntologyFromOntologyDocument(f)
          val ssName = f.getAbsolutePath.split("_StackSetModel.owl")(0).split("/").last
          if (o.getOntologyID.isAnonymous)
            o.getOWLOntologyManager
              .applyChange(
                new SetOntologyID(
                  o, new OWLOntologyID(ModelIRI.stackSetIRI(ssName),null)
                ))
          m.ontologies()
            .filter(!_.getOntologyID.getOntologyIRI.get().getIRIString.endsWith("stackset#"))
            .forEach(io => m.applyChange(new model.AddImport(o,
              df.getOWLImportsDeclaration(io.getOntologyID.getOntologyIRI.get))))

          m.applyChange(new model.AddImport(
            o, df.getOWLImportsDeclaration(IRI.create("http://www.w3.org/2002/07/owl#"))
          ))
        }))

    val o = m.loadOntologyFromOntologyDocument(new File(inputPath))


    val is = o.imports().flatMap(_.individualsInSignature())
    val ai: util.ArrayList[OWLIndividual] = new util.ArrayList[OWLIndividual]()
    is.forEach(i => ai.add(i))
    o.add(df.getOWLDifferentIndividualsAxiom(ai))

    val axiomsCountPrint = "[Logical Axioms Count: "+o.getLogicalAxiomCount(Imports.INCLUDED)+"]"

    print(f"\n\n ${RESET}${BOLD}${name}${RESET}\twas loaded." +
      f"\t\t${axiomsCountPrint}%-5s")
    (o,df,m,name)
  }




  private def fetchStackSetFiles(ssDir: File, infrName:String, inPath: String):
  (Vector[(String, Json, Option[Json])], String) =
  {
    val ssName = ssDir.getName
    val inputPath = inPath.replace("~",System.getProperty("user.home"))

    (ssDir
      .listFiles().toVector
      .filter( f => (
        !f.getName.contains("DS_Store")) &&
        !f.getName.endsWith("Descriptor.json"))
      .flatMap( templateFile =>
        jsonsFromFile(templateFile,infrName,inputPath,ssName)),
      ssName)
  }




  private def jsonsFromFile(templateFile: File,
                            infrastructureName: String,
                            inputPath: String,
                            stacksetName: String) =
  {

    val templateName  =
      templateFile.getName.split(".json").head
    val descrFileName =
      s"$inputPath$stacksetName/${templateName}Descriptor.json"
    var descrFile: Option[File] = None

    try
      descrFile = Some(new File(descrFileName))
    catch
      {
        case e: FileNotFoundException =>
          exitWithMessage(s"No Descriptor found for template $templateName. " +
            s"Descriptor is required in order to proceed.")
      }

    val jTemplate   =
      Parse.parseOption(Source.fromFile(templateFile).mkString)
    val jDescriptor =
      Parse.parseOption(Source.fromFile(descrFile.get).mkString)

    (jTemplate,jDescriptor) match {
      case (Some(jt),Some(_))
      => Vector((templateName, jt, jDescriptor))
      case _
      => logger.error(s"Unable to parse either " +
        s"Template or Descriptor file for stack $templateName)")
        Vector()
    }

  }



  private def createDescriptorForTemplateName(templateName: String, inputDir: String) :Unit = {
    val ds = "{\n  " +
      "\"AWS::Region\" : \"us-east-1\",\n  " +
      "\"AWS::Partition\" : \"aws\",\n  " +
      "\"AWS::StackId\" : \"111111\",\n  " +
      "\"AWS::AccountId\" : \"999999999999\",\n  " +
      "\"AWS::NotificationARNs\" : \"arn:aws:cnc:test:arn\",\n  " +
      "\"AWS::StackName\" : \"" + templateName + "\",\n  " +
      "\"AWS::URLSuffix\" : \".aws\"\n}"

    Files.write(
      Paths.get(inputDir + "/" + templateName + "Descriptor.json"),
      ds.getBytes(StandardCharsets.UTF_8)
    )
  }




  private def exitWithMessage(msg: String): Unit = {
    logger.error(msg)
    System.exit(0)
  }




}