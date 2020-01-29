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

package com.cloud.formal.reasoning

import com.cloud.formal.reasoning.PropertyType.PropertyType
import com.cloud.formal.reasoning.QueryBuildType.QueryBuildType
import com.cloud.formal.reasoning.QueryOutcome.QueryOutcome
import org.semanticweb.owlapi.model.OWLNamedIndividual
import org.semanticweb.owlapi.reasoner.NodeSet

import scala.collection.JavaConverters._
import Console.{BOLD, RESET}


sealed trait Property {
  val id: String
  val reqResTypes: Option[Vector[String]]
  val propType: PropertyType
  val propQuery: String
  val description: Option[String]
  val unsatPrint: Option[String]
  val sat0print: Option[String]
  val sat1print: Option[String]

  def truePrint(print: String) =
    s"$RESET$BOLD${Color.Green}TRUE$RESET,$print"
  def trueBWPrint(print: String) =
    s"TRUE,$print"

//  def unknownPrint(print: String) =
//    s"$RESET$BOLD${Color.LightRed}UNKNOWN$RESET\t$print"
  def unknownFalsePrint(print: String) =
    s"$RESET$BOLD${Color.LightRed}UNKNOWN\\FALSE$RESET,$print"
  def unknownFalseBWPrint(print: String) =
    s"UNKNOWN\\FALSE,$print"

  def unknownTruePrint(print: String) =
    s"$RESET$BOLD${Color.LightGreen}UNKNOWN\\TRUE$RESET,$print"
  def unknownTrueBWPrint(print: String) =
    s"UNKNOWN\\TRUE,$print"

  def falsePrint(print: String) =
    s"$RESET$BOLD${Color.Red}FALSE$RESET,$print"
  def falseBWPrint(print: String) =
    s"FALSE,$print"

  def getPassOrFilePrint(outcome: (QueryOutcome, Option[NodeSet[OWLNamedIndividual]])): String
  def getOutcomePrint(outcome: (QueryOutcome, Option[NodeSet[OWLNamedIndividual]])): String
}

case class TFFproperty(id: String,
                       reqResTypes: Option[Vector[String]],
                       queryBuildType: String,
                        instQuery: String,
                        propQuery:String,
                      description: Option[String],
                      unsatPrint: Option[String],
                      sat0print: Option[String],
                      sat1print: Option[String]) extends Property
{
  val propType: PropertyType = PropertyType.TFF

  override def getPassOrFilePrint(outcome: (QueryOutcome, Option[NodeSet[OWLNamedIndividual]])) =
    if (outcome._1==QueryOutcome.UNSAT) "PASS"
    else "FAIL"

  override def getOutcomePrint(outcome: (QueryOutcome, Option[NodeSet[OWLNamedIndividual]])): String = {
    outcome match {
      case (QueryOutcome.UNSAT, _) => trueBWPrint(unsatPrint.get)
      case (QueryOutcome.SAT0, _) => unknownFalseBWPrint(sat0print.get)
      case (QueryOutcome.SAT1, Some(s)) =>
        falseBWPrint(sat1print.get) +
          s.entities().iterator().asScala.
            foldLeft(",(")( (a,e) => a + e.getIRI.toString.split("#").last+" *** ")
            .dropRight(5) + ")"
    }
  }

}

case class TTFproperty(id: String,
                       reqResTypes: Option[Vector[String]],
                  propQuery:String,
                  description: Option[String],
                  unsatPrint: Option[String],
                  sat0print: Option[String],
                  sat1print: Option[String]) extends Property
{
  val propType: PropertyType = PropertyType.TTF

  override def getPassOrFilePrint(outcome: (QueryOutcome, Option[NodeSet[OWLNamedIndividual]])) =
    if (outcome._1==QueryOutcome.SAT1) "PASS"
    else "FAIL"

  override def getOutcomePrint(outcome: (QueryOutcome, Option[NodeSet[OWLNamedIndividual]])): String = {
    outcome match {
      case (QueryOutcome.UNSAT, _) => trueBWPrint(unsatPrint.get)
      case (QueryOutcome.SAT0, _) => trueBWPrint(sat0print.get)
      case (QueryOutcome.SAT1, Some(s)) =>
        falseBWPrint(sat1print.get) +
          s.entities().iterator().asScala.
            foldLeft(",(")( (a,e) => a + e.getIRI.toString.split("#").last+" *** ")
            .dropRight(5) + ")"
    }
  }
}

case class FTTproperty(id: String,
                       reqResTypes: Option[Vector[String]],
                       queryBuildType: String,
                       instQuery: String,
                  propQuery:String,
                  description: Option[String],
                  unsatPrint: Option[String],
                  sat0print: Option[String],
                  sat1print: Option[String]) extends Property
{
  val propType: PropertyType = PropertyType.FTT

  override def getPassOrFilePrint(outcome: (QueryOutcome, Option[NodeSet[OWLNamedIndividual]])) =
    if (outcome._1==QueryOutcome.UNSAT) "PASS"
    else "FAIL"

  override def getOutcomePrint(outcome: (QueryOutcome, Option[NodeSet[OWLNamedIndividual]])): String = {
    outcome match {
      case (QueryOutcome.UNSAT, _) =>
        falseBWPrint(unsatPrint.get)
      case (QueryOutcome.SAT0, _) => unknownTrueBWPrint(sat0print.get)
      case (QueryOutcome.SAT1, Some(s)) =>
        trueBWPrint(sat1print.get)  +
          s.entities().iterator().asScala.
            foldLeft(",(")( (a,e) => a + e.getIRI.toString.split("#").last+" *** ")
            .dropRight(5) + ")"
    }
  }
}

case class FFTproperty(id: String,
                       reqResTypes: Option[Vector[String]],
                  propQuery:String,
                  description: Option[String],
                  unsatPrint: Option[String],
                  sat0print: Option[String],
                  sat1print: Option[String]) extends Property
{
  val propType: PropertyType = PropertyType.FFT

  override def getPassOrFilePrint(outcome: (QueryOutcome, Option[NodeSet[OWLNamedIndividual]])) =
    if (outcome._1==QueryOutcome.SAT1) "PASS"
    else "FAIL"

  override def getOutcomePrint(outcome: (QueryOutcome, Option[NodeSet[OWLNamedIndividual]])): String = {
    outcome match {
      case (QueryOutcome.UNSAT, _) =>
        falseBWPrint(unsatPrint.get)
      case (QueryOutcome.SAT0, _) => falseBWPrint(sat0print.get)
      case (QueryOutcome.SAT1, Some(s)) =>
        trueBWPrint(sat1print.get)  +
          s.entities().iterator().asScala.
            foldLeft(",(")( (a,e) => a + e.getIRI.toString.split("#").last+" *** ")
            .dropRight(5) + ")"
    }
  }
}


