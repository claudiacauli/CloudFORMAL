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

package com.cloud.formal.binding

import java.io.{File, FileNotFoundException}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util

import argonaut.{Json, Parse}
import com.cloud.formal.{FileSuffix, Extension, FilePath, Ontology, SysUtil}
import com.cloud.formal.mapping.specifications.ResourceSpecificationModel
import com.cloud.formal.mapping.templates.mapping.InfrastructureModel
import com.cloud.formal.mapping.templates.{Infrastructure, Json2InfrastructureEncoder}
import com.cloud.formal.model.{Model, ModelFileSuffix, ModelIRI}
import com.typesafe.scalalogging.LazyLogging
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model
import org.semanticweb.owlapi.model._

import Console.{BOLD, RESET}
import scala.io.Source



object Interface extends LazyLogging{




  def compileAndSaveSpecification(inPath: String, printEnabled: Boolean = true): Unit = {

    val inputPath = inPath.replace("~",System.getProperty(SysUtil.UserHome))
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
      if (printEnabled) println(s" [Spec-OWL] ${sm.name}")
      val dir = new File(FilePath.ResourceTerms)
      if (!dir.exists())
        dir.mkdir()
      sm.writeToOutputFolder(FilePath.ResourceTerms)
    })

  }


  private[formal] def createInfrastructure(file: File, inputPath: String, outputPath: String)
  : (String, OWLOntology, OWLDataFactory, OWLOntologyManager) = {
    val infrastructureName = file.getName

    println("\n "+ infrastructureName)

    //  val p = Benchmarking.timeN(10, 100)("Encoding", encodeInfrastructure(file,infrastructureName))
    val p = encodeInfrastructure(file,infrastructureName, inputPath)
    val i = p._1
    val im = p._2

    println(s" - [Encoding] Resources count: ${i.getResourcesCount}")
    println(s" - [Encoding] Resource types count: ${i.getResourceTypesCount}")

    im.writeToOutputFolder(outputPath)
    i.writeInfrastructureSummaryToFolder(outputPath)
    (im.name, im.ontology, im.df, im.manager)
  }



  private[formal]
  def encodeInfrastructure(file: File, infrastructureName: String, inputPath: String): (Infrastructure, Model) = {

    val i =
      Json2InfrastructureEncoder.encode(
        file.listFiles()
          .filter(_.isDirectory)
          .map ( f =>
            createStackSetFiles(f,infrastructureName, inputPath)).toVector,
        infrastructureName)

    val im =
      InfrastructureModel.fromInfrastructure(i)

    (i,im)
  }




  private def createStackSetFiles(file: File, iN:String, inputPath:String):
  (Vector[(String, Json, Option[Json])], String) =
  {
    val stackSetName = file.getName

    (file.listFiles().toVector
      .filter(_.getAbsolutePath.endsWith(Extension.Json))
      flatMap (f => {
      val templateName = f.getName.split(Extension.Json).head
      if (!templateName
        .endsWith(FileSuffix.Descriptor) && !templateName.endsWith(SysUtil.DSStore))
      {
        var descriptor: File = null
        try {
          descriptor = new File(inputPath + "/" + iN +"/" +stackSetName + "/" + templateName + FileSuffix.DescriptorJson)
          if (!descriptor.exists)
            createDescriptorForTemplateName(templateName, file.getAbsolutePath)
        } catch {
          case e: FileNotFoundException => descriptor = null
        }
        val tmplJson  = Parse.parseOption(Source.fromFile(f).mkString).get
        val descrJson = Parse.parseOption(Source.fromFile(descriptor).mkString)
        Vector((templateName, tmplJson, descrJson))
      } else Vector()
    }),stackSetName )
  }




  def compileAndSaveTemplates(inPath: String,
                              infrastructureCreationFunction: (File,String, String) => (String, OWLOntology, OWLDataFactory, OWLOntologyManager),
                              outputPath: String = FilePath.BenchmarksOut)
  : (String, OWLOntology, OWLDataFactory, OWLOntologyManager) = {

    val inputPath = inPath.replace("~",System.getProperty(SysUtil.UserHome))

    new File(inputPath)
      .listFiles()
      .filter(_.isDirectory)
      .map(f => infrastructureCreationFunction(f, inputPath, outputPath))
      .head

  }



  def modelAndSaveAllTemplates(values: Array[String],
    infrastructureCreationFunction: (File,String, String) => (String, OWLOntology, OWLDataFactory, OWLOntologyManager)) :
   Vector[(String, OWLOntology, OWLDataFactory, OWLOntologyManager)] = {

    val inputPath = values(0).replace("~",System.getProperty(SysUtil.UserHome))

    val outputPath =
      if (values.size == 2)
        values(1).replace("~", System.getProperty(SysUtil.UserHome))
      else
        FilePath.BenchmarksOut

    new File(inputPath)
      .listFiles()
      .filter(_.isDirectory)
      .map(iF => modelAndSaveTemplates(Array(iF.getAbsolutePath,outputPath),infrastructureCreationFunction))
      .toVector
  }


  def modelAndSaveTemplates(values: Array[String],
      infrastructureCreationFunction: (File,String, String) => (String, OWLOntology, OWLDataFactory, OWLOntologyManager))
  : (String, OWLOntology, OWLDataFactory, OWLOntologyManager) = {

    val inputPath = values(0).replace("~",System.getProperty(SysUtil.UserHome))

    val outputPath =
      if (values.size == 2)
        values(1).replace("~", System.getProperty(SysUtil.UserHome))
      else
        FilePath.BenchmarksOut

     compileAndSaveTemplates(inputPath, infrastructureCreationFunction, outputPath)
  }



  def loadModel(inPath: String, printEnabled: Boolean = true): (OWLOntology, OWLDataFactory, OWLOntologyManager, String) = {
    val inputPath = inPath.replace("~",System.getProperty(SysUtil.UserHome))
    val m = OWLManager.createOWLOntologyManager()
    val df = m.getOWLDataFactory

    val name = inputPath.split(ModelFileSuffix.Infrastructure)(0).split("/").last

    val preDir = new File( inPath.split("/").dropRight(1).mkString("/") )

    if (printEnabled) print(s"\n******************************************" +
      s"*******************************************")

    preDir.listFiles().filter(_.isDirectory)
      .foreach(_.listFiles().filter(f => f.getName.endsWith(Extension.Owl) && !f.getName.endsWith(ModelFileSuffix.StackSet))
        .foreach( f => {
          m.loadOntologyFromOntologyDocument(f)
        }))

    preDir.listFiles().filter(_.isDirectory)
      .foreach(_.listFiles().filter(_.getName.endsWith(ModelFileSuffix.StackSet))
        .foreach( f => {
          val o = m.loadOntologyFromOntologyDocument(f)
          val ssName = f.getAbsolutePath.split(ModelFileSuffix.StackSet)(0).split("/").last
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
            o, df.getOWLImportsDeclaration(IRI.create(Ontology.OWLOntologyStringIRI))
          ))
        }))

    val o = m.loadOntologyFromOntologyDocument(new File(inputPath))


    val is = o.imports().flatMap(_.individualsInSignature())
    val ai: util.ArrayList[OWLIndividual] = new util.ArrayList[OWLIndividual]()
    is.forEach(i => ai.add(i))
    o.add(df.getOWLDifferentIndividualsAxiom(ai))

    if (printEnabled) print(f"\n\n $RESET$BOLD$name$RESET\twas loaded.")
    (o,df,m,name)
  }




  private def fetchStackSetFiles(ssDir: File, infrName:String, inPath: String):
  (Vector[(String, Json, Option[Json])], String) =
  {
    val ssName = ssDir.getName
    val inputPath = inPath.replace("~",System.getProperty(SysUtil.UserHome))

    (ssDir
      .listFiles().toVector
      .filter( f => (
        !f.getName.contains(SysUtil.DSStore)) &&
        !f.getName.endsWith(FileSuffix.DescriptorJson))
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
      templateFile.getName.split(Extension.Json).head
    val descrFileName =
      s"$inputPath$stacksetName/$templateName${FileSuffix.DescriptorJson}"
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
      Paths.get(inputDir + "/" + templateName + FileSuffix.DescriptorJson),
      ds.getBytes(StandardCharsets.UTF_8)
    )
  }




  private def exitWithMessage(msg: String): Unit = {
    logger.error(msg)
    System.exit(0)
  }




}
