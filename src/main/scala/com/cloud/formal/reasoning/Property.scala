package com.cloud.formal.reasoning

import com.cloud.formal.reasoning.PropertyType.PropertyType
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
    s"${RESET}${BOLD}${Color.Green}TRUE${RESET}\t${print}"

  def unknownPrint(print: String) =
    s"${RESET}${BOLD}${Color.LightRed}UNKNOWN${RESET}\t${print}"

  def falsePrint(print: String) =
    s"${RESET}${BOLD}${Color.Red}FALSE${RESET}\t${print}"

  def getOutcomePrint(outcome: (QueryOutcome, Option[NodeSet[OWLNamedIndividual]])): String
}

case class TFFproperty(val id: String,
                       val reqResTypes: Option[Vector[String]],
                val instQuery: String,
                  val propQuery:String,
                  val description: Option[String],
                  val unsatPrint: Option[String],
                  val sat0print: Option[String],
                  val sat1print: Option[String]) extends Property
{
  val propType: PropertyType = PropertyType.TFF

  override def getOutcomePrint(outcome: (QueryOutcome, Option[NodeSet[OWLNamedIndividual]])): String = {
    outcome match {
      case (QueryOutcome.UNSAT, _) => truePrint(unsatPrint.get)
      case (QueryOutcome.SAT0, _) => unknownPrint(sat0print.get)
      case (QueryOutcome.SAT1, Some(s)) =>
        falsePrint(sat1print.get) +
          s.entities().iterator().asScala.
            foldLeft("\n\t\t\t(")( (a,e) => a + e.getIRI.toString.split("#").last+", ")
            .dropRight(2) + ")"
    }
  }

}

case class TTFproperty(val id: String,
                       val reqResTypes: Option[Vector[String]],
                  val propQuery:String,
                  val description: Option[String],
                  val unsatPrint: Option[String],
                  val sat0print: Option[String],
                  val sat1print: Option[String]) extends Property
{
  val propType: PropertyType = PropertyType.TTF

  override def getOutcomePrint(outcome: (QueryOutcome, Option[NodeSet[OWLNamedIndividual]])): String = {
    outcome match {
      case (QueryOutcome.UNSAT, _) => truePrint(unsatPrint.get)
      case (QueryOutcome.SAT0, _) => truePrint(sat0print.get)
      case (QueryOutcome.SAT1, Some(s)) =>
        falsePrint(sat1print.get) +
          s.entities().iterator().asScala.
            foldLeft("\n\t\t\t(")( (a,e) => a + e.getIRI.toString.split("#").last+", ")
            .dropRight(2) + ")"
    }
  }
}

case class FTTproperty(val id: String,
                       val reqResTypes: Option[Vector[String]],
                  val instQuery: String,
                  val propQuery:String,
                  val description: Option[String],
                  val unsatPrint: Option[String],
                  val sat0print: Option[String],
                  val sat1print: Option[String]) extends Property
{
  val propType: PropertyType = PropertyType.FTT

  override def getOutcomePrint(outcome: (QueryOutcome, Option[NodeSet[OWLNamedIndividual]])): String = {
    outcome match {
      case (QueryOutcome.UNSAT, _) =>
        falsePrint(unsatPrint.get)
      case (QueryOutcome.SAT0, _) => truePrint(sat0print.get)
      case (QueryOutcome.SAT1, Some(s)) =>
        truePrint(sat1print.get)
    }
  }
}

case class FFTproperty(val id: String,
                       val reqResTypes: Option[Vector[String]],
                  val propQuery:String,
                  val description: Option[String],
                  val unsatPrint: Option[String],
                  val sat0print: Option[String],
                  val sat1print: Option[String]) extends Property
{
  val propType: PropertyType = PropertyType.FFT
  override def getOutcomePrint(outcome: (QueryOutcome, Option[NodeSet[OWLNamedIndividual]])): String = {
    outcome match {
      case (QueryOutcome.UNSAT, _) =>
        falsePrint(unsatPrint.get)
      case (QueryOutcome.SAT0, _) => falsePrint(sat0print.get)
      case (QueryOutcome.SAT1, Some(s)) =>
        truePrint(sat1print.get)
    }
  }
}


