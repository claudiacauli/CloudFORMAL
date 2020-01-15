package com.cloud.formal.reasoning

import java.util

import org.semanticweb.owlapi.expression.ShortFormEntityChecker
import org.semanticweb.owlapi.manchestersyntax.parser.ManchesterOWLSyntaxClassExpressionParser
import org.semanticweb.owlapi.manchestersyntax.renderer.ParserException
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.util.{BidirectionalShortFormProviderAdapter, SimpleShortFormProvider}

import scala.collection.JavaConverters._
import Console.{RESET,BOLD}


class PropertyEvaluator(val r: Reasoner, val o: OWLOntology, val df: OWLDataFactory, val m: OWLOntologyManager) {


  def evalAndPrint(p: Property): Unit = {

    try {
      println(s"\n\t$RESET$BOLD*$RESET ${p.description.get}")
      val t = System.currentTimeMillis()
      val outcome = r.runQuery( parseStringDLQuery(makeQuery(p)))
      print("\t\t[" + (System.currentTimeMillis()-t) + " ms] ")
      println(p.getOutcomePrint(outcome))
    } catch {
      case e: ParserException =>
        println("\t\t Could not parse the string expression " + makeQuery(p))
        println(e.getMessage)
    }

  }


  def makeQuery(p: Property): String = {

    def disjunct (iq: String, pq: String) = {

      r.getInstances(parseStringDLQuery(iq))
        .entities().iterator().asScala
        .foldLeft("")(
          (a, i) => a + "(" + replaceIndividualVariableName(i, pq) + ") or ")
        .dropRight(4)
    }

    p match {
      case TFFproperty(_,iq,pq,_,_,_,_) => disjunct(iq,pq)
      case FTTproperty(_,iq,pq,_,_,_,_) => disjunct(iq,pq)
      case x => x.propQuery
    }

  }



  def parseStringDLQuery(dlQuery: String): OWLClassExpression =
  {
    var importClosure: util.ArrayList[OWLOntology] =
      new util.ArrayList[OWLOntology]()
    o.importsClosure().forEach(o =>
      importClosure.add(o))

    val entityChecker = new ShortFormEntityChecker(
      new BidirectionalShortFormProviderAdapter(
        m,
        importClosure,
        new SimpleShortFormProvider()))

    val parser =
      new ManchesterOWLSyntaxClassExpressionParser(
        df,
        entityChecker)

    if (dlQuery.equals(""))
      return df.getOWLNothing
    else
      parser.parse(dlQuery)
  }

  private def replaceIndividualVariableName(i: OWLNamedIndividual, s: String): String
  = s.replaceAll("\\$\\{x\\}", i.getIRI.getFragment)

}
