package com.cloud.formal

import argonaut.Json

package object reasoning {


  private[reasoning]
  object PropertyType extends Enumeration {
    type PropertyType = String
    final val TFF: PropertyType = "TFF"
    final val TTF: PropertyType = "TTF"
    final val FTT: PropertyType = "FTT"
    final val FFT: PropertyType = "FFT"
  }

  private[reasoning]
  object QueryOutcome extends Enumeration {
    type QueryOutcome = String
    final val UNSAT: QueryOutcome = "UNSAT"
    final val SAT0: QueryOutcome  = "SAT/0"
    final val SAT1: QueryOutcome  = "SAT/+"
  }

  private[reasoning]
  object Tag {
    val ManifestPath = "src/main/scala/com/cloud/formal/reasoning/properties/manifest.json"
    val ID = "ID"
    val Type = "type"
    val ReqResourceTypes = "requiredResourceTypes"
    val PropertyQuery = "propQuery"
    val InstanceQuery = "instQuery"
    val Description = "description"
    val UnsatPrint = "unsatPrint"
    val Sat0Print = "sat0print"
    val Sat1Print = "sat1print"
  }

  private[reasoning] object Color extends Enumeration{
    final val Red = "\u001b[31m"
    final val LightRed = "\u001b[91m"
    final val LightGreen = "\u001b[92m"
    final val Green = "\u001b[32m"
  }

  private[reasoning]
  def exitWithMessage(msg: String): Unit = {
    println(msg)
    System.exit(0)
  }

  private[reasoning]
  def getStringField(j: Json, tag: String): String =
    j.field(tag).get match {
      case js if js.isString => js.string.get
      case ja if ja.isArray  => ja.array.get.foldLeft("")((a, j) => a + j.string.get)
    }


  private[reasoning]
  def getOptionalField(j: Json, fieldName: String) =
    j.field(fieldName) match {
      case None => None
      case Some(js) => Some(getStringField(j, fieldName))
    }

  private[reasoning]
  def getArrayField(j: Json, fieldName: String) =
    j.field(fieldName) match {
      case None => None
      case Some(ja) => Some(ja.array.get.map(_.string.get).toVector)
    }



}
