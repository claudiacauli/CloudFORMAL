package com.cloud.formal.reasoning

import java.io.File

import argonaut.{Json, Parse}

import scala.io.Source


class PropertiesGenerator {

  val propMap: Map[String, Property] = init()

  def getProperty(id: String): Option[Property] = propMap.get(id)

  def init(): Map[String,Property] = {
    Parse.parseOption(Source.fromFile(new File(Tag.ManifestPath)).mkString)
    match {
      case None => exitWithMessage("Could not parse manifest file. Must be invalid.")
        Map()
      case Some(j) => j.array.get.flatMap(j => parsePropertiesFile(j.string.get)).toMap
    }
  }


  private def parsePropertiesFile(filePath: String): Map[String, Property] = {

    Parse.parseOption(
      Source.fromFile(
        new File(filePath)).mkString) match {
      case None => exitWithMessage("Property json file " + filePath + " could not be parsed.")
        Map()
      case Some(j) => j.array.get.flatMap(parseProperty).toMap
    }

  }

  private def parseProperty(j: Json): Map[String, Property] = {

    val id    = getStringField(j,Tag.ID)
    val iq    = getOptionalField(j, Tag.InstanceQuery)
    val pq    = getStringField(j,Tag.PropertyQuery)
    val d     = getOptionalField(j, Tag.Description)
    val uPr   = getOptionalField(j, Tag.UnsatPrint)
    val s0Pr  = getOptionalField(j, Tag.Sat0Print)
    val s1Pr  = getOptionalField(j, Tag.Sat1Print)

    j.field(Tag.Type).get.string.get match {
      case PropertyType.TFF => Map(id -> new TFFproperty(id, iq.get, pq, d, uPr, s0Pr, s1Pr))
      case PropertyType.TTF => Map(id -> new TTFproperty(id, pq, d, uPr, s0Pr, s1Pr))
      case PropertyType.FTT => Map(id -> new FTTproperty(id, iq.get, pq, d, uPr, s0Pr, s1Pr))
      case PropertyType.FFT => Map(id -> new FFTproperty(id, pq, d, uPr, s0Pr, s1Pr))
    }

  }



}
