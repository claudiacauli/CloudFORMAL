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

package com.cloud.formal

import argonaut.Json
import Console.{GREEN,RED,RESET}

package object reasoning {


  private[formal]
  object PropertyType extends Enumeration
  {
    type PropertyType = String
    final val TFF: PropertyType = "TFF"
    final val TTF: PropertyType = "TTF"
    final val FTT: PropertyType = "FTT"
    final val FFT: PropertyType = "FFT"
  }

  private[formal]
  object QueryOutcome extends Enumeration
  {
    type QueryOutcome = String
    final val UNSAT: QueryOutcome = "UNSAT"
    final val SAT0: QueryOutcome  = "SAT/0"
    final val SAT1: QueryOutcome  = "SAT/+"
  }


  private[formal]
  object PassFailOutcome extends Enumeration
  {
    final val PASS = "PASS"
    final val FAIL = "FAIL"
    final val greenPASS = s"${GREEN}PASS$RESET"
    final val redFAIL = s"${RED}FAIL$RESET"
  }

  private[formal]
  object FourValues extends Enumeration
  {
    final val TRUE          = "TRUE"
    final val FALSE         = "FALSE"
    final val MAYBE_TRUE    = "UNKNOWN-TRUE"
    final val MAYBE_FALSE   = "UNKNOWN-FALSE"
  }


  private[formal]
  object QueryBuildType extends Enumeration
  {
    type QueryBuildType = String
    final val NominalProp: QueryBuildType   = "nominalProp"
    final val DisjunctProp: QueryBuildType  = "disjunctProp"
  }

  private[reasoning]
  object Tag
  {
    val ManifestPath      = "src/main/scala/com/cloud/formal/reasoning/properties/manifest.json"
    val ID                = "ID"
    val Type              = "type"
    val QueryBuildType    = "queryBuildType"
    val ReqResourceTypes  = "requiredResourceTypes"
    val PropertyQuery     = "propQuery"
    val InstanceQuery     = "instQuery"
    val Description       = "description"
    val UnsatPrint        = "unsatPrint"
    val Sat0Print         = "sat0print"
    val Sat1Print         = "sat1print"
  }

  private[reasoning] object Color extends Enumeration
  {
    final val Red         = "\u001b[31m"
    final val LightRed    = "\u001b[91m"
    final val LightGreen  = "\u001b[92m"
    final val Green       = "\u001b[32m"
  }

  private[reasoning]
  def exitWithMessage(msg: String): Unit =
  {
    println(msg)
    System.exit(0)
  }

  private[reasoning]
  def getStringField(j: Json, tag: String): String =
    j.field(tag).get match
    {
      case js if js.isString => js.string.get
      case ja if ja.isArray  => ja.array.get.foldLeft("")((a, j) => a + j.string.get)
    }


  private[reasoning]
  def getOptionalField(j: Json, fieldName: String) =
    j.field(fieldName) match
    {
      case None => None
      case Some(js) => Some(getStringField(j, fieldName))
    }

  private[reasoning]
  def getArrayField(j: Json, fieldName: String) =
    j.field(fieldName) match
    {
      case None => None
      case Some(ja) => Some(ja.array.get.map(_.string.get).toVector)
    }



}
