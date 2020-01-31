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
import com.cloud.formal.reasoning.QueryOutcome.QueryOutcome
import org.semanticweb.owlapi.model.OWLNamedIndividual
import org.semanticweb.owlapi.reasoner.NodeSet

import scala.jdk.CollectionConverters._
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
    s"$BOLD${Color.Green}${FourValues.TRUE}$RESET,$print"
  def trueBWPrint(print: String) =
    s"${FourValues.TRUE},$print,"

  def unknownFalsePrint(print: String) =
    s"$BOLD${Color.LightRed}${FourValues.MAYBE_FALSE}$RESET,$print"
  def unknownFalseBWPrint(print: String) =
    s"${FourValues.MAYBE_FALSE},$print,"

  def unknownTruePrint(print: String) =
    s"$BOLD${Color.LightGreen}${FourValues.MAYBE_TRUE}$RESET,$print"
  def unknownTrueBWPrint(print: String) =
    s"${FourValues.MAYBE_TRUE},$print,"

  def falsePrint(print: String) =
    s"$BOLD${Color.Red}${FourValues.FALSE}$RESET,$print"
  def falseBWPrint(print: String) =
    s"${FourValues.FALSE},$print,"

  def getPassOrFilePrint(color: Boolean, outcome: (QueryOutcome, Option[NodeSet[OWLNamedIndividual]])): String
  def getTrueOrFalsePrint(outcome: (QueryOutcome, Option[NodeSet[OWLNamedIndividual]])): String
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

  override def getPassOrFilePrint(color: Boolean, outcome: (QueryOutcome, Option[NodeSet[OWLNamedIndividual]])) =
    if      (!color && outcome._1==QueryOutcome.UNSAT) PassFailOutcome.PASS
    else if (!color) PassFailOutcome.FAIL
    else if (color && outcome._1==QueryOutcome.UNSAT) PassFailOutcome.greenPASS
    else    PassFailOutcome.redFAIL

  override def getTrueOrFalsePrint(outcome:(QueryOutcome, Option[NodeSet[OWLNamedIndividual]])) = {
    outcome._1 match {
      case QueryOutcome.UNSAT => FourValues.TRUE
      case QueryOutcome.SAT0  => FourValues.MAYBE_FALSE
      case QueryOutcome.SAT1  => FourValues.FALSE
    }
  }

  override def getOutcomePrint(outcome: (QueryOutcome, Option[NodeSet[OWLNamedIndividual]])): String = {
    outcome match {
      case (QueryOutcome.UNSAT, _) => trueBWPrint(unsatPrint.get)+ "N/A"
      case (QueryOutcome.SAT0, _) => unknownFalseBWPrint(sat0print.get)+ "N/A"
      case (QueryOutcome.SAT1, Some(s)) =>
        falseBWPrint(sat1print.get) +
          s.entities().iterator().asScala.
            foldLeft("(")( (a,e) => a + e.getIRI.toString.split("#").last+" *** ")
            .dropRight(5) + ")"
      case _ => ""
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

  override def getPassOrFilePrint(color: Boolean, outcome: (QueryOutcome, Option[NodeSet[OWLNamedIndividual]])) =
    if      (!color && outcome._1==QueryOutcome.SAT1) PassFailOutcome.PASS
    else if (!color) PassFailOutcome.FAIL
    else if (color && outcome._1==QueryOutcome.SAT1) PassFailOutcome.greenPASS
    else    PassFailOutcome.redFAIL

  override def getTrueOrFalsePrint(outcome:(QueryOutcome, Option[NodeSet[OWLNamedIndividual]])) = {
    outcome._1 match {
      case QueryOutcome.UNSAT => FourValues.TRUE
      case QueryOutcome.SAT0  => FourValues.TRUE
      case QueryOutcome.SAT1  => FourValues.FALSE
    }
  }

  override def getOutcomePrint(outcome: (QueryOutcome, Option[NodeSet[OWLNamedIndividual]])): String = {
    outcome match {
      case (QueryOutcome.UNSAT, _) => trueBWPrint(unsatPrint.get)+ "N/A"
      case (QueryOutcome.SAT0, _) => trueBWPrint(sat0print.get)+ "N/A"
      case (QueryOutcome.SAT1, Some(s)) =>
        falseBWPrint(sat1print.get) +
          s.entities().iterator().asScala.
            foldLeft("(")( (a,e) => a + e.getIRI.toString.split("#").last+" *** ")
            .dropRight(5) + ")"
      case _ => ""

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

  override def getPassOrFilePrint(color: Boolean, outcome: (QueryOutcome, Option[NodeSet[OWLNamedIndividual]])) =
    if      (!color && outcome._1==QueryOutcome.UNSAT) PassFailOutcome.PASS
    else if (!color) PassFailOutcome.FAIL
    else if (color && outcome._1==QueryOutcome.UNSAT) PassFailOutcome.greenPASS
    else    PassFailOutcome.redFAIL

  override def getTrueOrFalsePrint(outcome:(QueryOutcome, Option[NodeSet[OWLNamedIndividual]])) = {
    outcome._1 match {
      case QueryOutcome.UNSAT => FourValues.FALSE
      case QueryOutcome.SAT0  => FourValues.MAYBE_TRUE
      case QueryOutcome.SAT1  => FourValues.TRUE
    }
  }

  override def getOutcomePrint(outcome: (QueryOutcome, Option[NodeSet[OWLNamedIndividual]])): String = {
    outcome match {
      case (QueryOutcome.UNSAT, _) =>
        falseBWPrint(unsatPrint.get)+ "N/A"
      case (QueryOutcome.SAT0, _) => unknownTrueBWPrint(sat0print.get)+ "N/A"
      case (QueryOutcome.SAT1, Some(s)) =>
        trueBWPrint(sat1print.get)  +
          s.entities().iterator().asScala.
            foldLeft("(")( (a,e) => a + e.getIRI.toString.split("#").last+" *** ")
            .dropRight(5) + ")"
      case _ => ""

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

  override def getPassOrFilePrint(color: Boolean, outcome: (QueryOutcome, Option[NodeSet[OWLNamedIndividual]])) =
    if      (!color && outcome._1==QueryOutcome.SAT1) PassFailOutcome.PASS
    else if (!color) PassFailOutcome.FAIL
    else if (color && outcome._1==QueryOutcome.SAT1) PassFailOutcome.greenPASS
    else    PassFailOutcome.redFAIL

  override def getTrueOrFalsePrint(outcome:(QueryOutcome, Option[NodeSet[OWLNamedIndividual]])) = {
    outcome._1 match {
      case QueryOutcome.UNSAT => FourValues.FALSE
      case QueryOutcome.SAT0  => FourValues.FALSE
      case QueryOutcome.SAT1  => FourValues.TRUE
    }
  }

  override def getOutcomePrint(outcome: (QueryOutcome, Option[NodeSet[OWLNamedIndividual]])): String = {
    outcome match {
      case (QueryOutcome.UNSAT, _) =>
        falseBWPrint(unsatPrint.get) + "N/A"
      case (QueryOutcome.SAT0, _) => falseBWPrint(sat0print.get) + "N/A"
      case (QueryOutcome.SAT1, Some(s)) =>
        trueBWPrint(sat1print.get)  +
          s.entities().iterator().asScala.
            foldLeft("(")( (a,e) => a + e.getIRI.toString.split("#").last+" *** ")
            .dropRight(5) + ")"
      case _ => ""

    }
  }
}


