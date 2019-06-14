package com.claudiacauli.www.cloudformal.mapping.specifications

import java.io.{File, FileNotFoundException}

import argonaut.Parse
import aws.cfn.Extension
import aws.cfn.model.{Model, ModelIRI, ModelWriter}
import aws.cfn.mapping.Specification
import com.claudiacauli.www.cloudformal.model.{Model, ModelIRI, ModelWriter}
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{OWLDataFactory, OWLOntology, OWLOntologyManager}

import scala.io.Source

object ResourceSpecificationModel
{

  def fromResourceSpecification(resSpec: ResourceSpecification): Model =
    ResourceSpecificationModelMapper
      .fromSpecification(resSpec)


  def fromResourceSpecificationFile(f: File): Model = {

    val fileName = f.getName
        .split(Specification.SpecificationTrailer+Extension.Json)
        .head

      Parse.parseOption(Source.fromFile(f).mkString) match {
        case None     =>
          println("Parsing of Json from file " + f.getName + " failed.")
          sys.exit(0)
        case Some(j)  =>
          fromResourceSpecification(
            ResourceSpecificationMapper.fromJson(j,fileName))
      }
  }

  def fromResourceSpecificationFilePath(filePath: String): Model = {
    try {
      val f = new File(filePath)
      fromResourceSpecificationFile(f)
    } catch {
      case e:FileNotFoundException =>
        throw new FileNotFoundException("A file with path " + filePath + " was not found.\n" + e.getMessage)
    }
  }


}



private class ResourceSpecificationModel
(val name : String)
  extends Model
{

  val manager: OWLOntologyManager = OWLManager.createOWLOntologyManager()
  val ontology: OWLOntology = manager.createOntology(ModelIRI.resourceTerminologyIRI(name))
  val df: OWLDataFactory = manager.getOWLDataFactory

  def writeToOutputFolder (destinationFolder: String): Unit =
    ModelWriter
      .writeSpecificationToFolder(this,destinationFolder)



}
