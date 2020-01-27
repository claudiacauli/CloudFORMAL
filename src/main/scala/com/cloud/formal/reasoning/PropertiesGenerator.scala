package com.cloud.formal.reasoning

import java.io.File

import argonaut.{Json, Parse}

import scala.io.Source


object PropertiesGenerator {

  //val propVec: Vector[(String, Property)] = init()

  def init(): Vector[(String,Property)] = {
    Parse.parseOption(Source.fromFile(new File(Tag.ManifestPath)).mkString)
    match {
      case None => exitWithMessage("Could not parse manifest file. Must be invalid.")
        Vector()
      case Some(j) =>
        val propsVec = j.array.get.flatMap(propFilePath => parsePropertiesFile(propFilePath.string.get)).toVector
//        println("TOTAL Properties " + propsVec.size)
//        println("\t TFF " + propsVec.count(_._2.propType=="TFF"))
//        println("\t TTF " + propsVec.count(_._2.propType=="TTF"))
//        println("\t FTT " + propsVec.count(_._2.propType=="FTT"))
//        println("\t FFT " + propsVec.count(_._2.propType=="FFT"))
        propsVec
    }
  }


  private def parsePropertiesFile(filePath: String): Vector[(String,Property)]= {

    Parse.parseOption(
      Source.fromFile(
        new File(filePath)).mkString) match {
      case None => exitWithMessage("Property json file " + filePath + " could not be parsed.")
        Vector()
      case Some(j) => j.array.get.map(parseProperty).toVector
    }

  }

  private def parseProperty(j: Json): (String, Property) = {

    val id    = getStringField(j,Tag.ID)
    val qbt   = getOptionalField(j,Tag.QueryBuildType)
    val req   = getArrayField(j,Tag.ReqResourceTypes)
    val iq    = getOptionalField(j, Tag.InstanceQuery)
    val pq    = getStringField(j,Tag.PropertyQuery)
    val d     = getOptionalField(j, Tag.Description)
    val uPr   = getOptionalField(j, Tag.UnsatPrint)
    val s0Pr  = getOptionalField(j, Tag.Sat0Print)
    val s1Pr  = getOptionalField(j, Tag.Sat1Print)

    j.field(Tag.Type).get.string.get match {
      case PropertyType.TFF => (id, TFFproperty(id, req, qbt.get, iq.get, pq, d, uPr, s0Pr, s1Pr))
      case PropertyType.TTF => (id, TTFproperty(id, req, pq, d, uPr, s0Pr, s1Pr))
      case PropertyType.FTT => (id, FTTproperty(id, req, qbt.get, iq.get, pq, d, uPr, s0Pr, s1Pr))
      case PropertyType.FFT => (id, FFTproperty(id, req, pq, d, uPr, s0Pr, s1Pr))
    }

  }



}
