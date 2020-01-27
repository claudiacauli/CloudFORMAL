package com.cloud.formal.reasoning

import java.util

import com.cloud.formal.reasoning.QueryBuildType.QueryBuildType
import com.cloud.formal.reasoning.QueryOutcome.QueryOutcome
import org.semanticweb.owlapi.expression.ShortFormEntityChecker
import org.semanticweb.owlapi.manchestersyntax.parser.ManchesterOWLSyntaxClassExpressionParser
import org.semanticweb.owlapi.manchestersyntax.renderer.ParserException
import org.semanticweb.owlapi.model.{OWLClassExpression, _}
import org.semanticweb.owlapi.reasoner.NodeSet
import org.semanticweb.owlapi.util.{BidirectionalShortFormProviderAdapter, SimpleShortFormProvider}

import scala.collection.JavaConverters._


class PropertyEval(val r: Reasoner, val o: OWLOntology, val df: OWLDataFactory, val m: OWLOntologyManager) {


  private[formal]
  def evalAndPrint(p: Property, printEnabled: Boolean)
  (runQueryFun: ((=> OWLClassExpression) => Boolean) => (=>OWLClassExpression) => (QueryOutcome,Option[NodeSet[OWLNamedIndividual]]) )
  (satCheckFun: Boolean => (=>OWLClassExpression) => Boolean) = {

    try {
//      if (printEnabled && hasRequiredResourceTypes(o,p))
//        println(p.id+"\t" + makeQuery(p))
      val outcome = evalProperty(p)(runQueryFun)(satCheckFun)
      if (printEnabled) println(p.id + "," + p.getOutcomePrint(outcome))
      Some(outcome)
    } catch {
      case e: ParserException =>
        println("\t\t Could not parse the string expression " + makeQuery(p))
        println(e.getMessage)
        None
    }

  }



  private def evalProperty(p: Property)
                          (runQueryFun: ((=> OWLClassExpression) => Boolean) => (=>OWLClassExpression) => (QueryOutcome,Option[NodeSet[OWLNamedIndividual]]) )
                          (satCheckFun: Boolean => (=>OWLClassExpression) => Boolean):
  (QueryOutcome,Option[NodeSet[OWLNamedIndividual]]) =
  {
      runQueryFun(satCheckFun(hasRequiredResourceTypes(o,p)))(parseStringDLQuery(makeQuery(p)))
  }



  private[formal]
  def makeQuery(p: Property): String = {

    def disjunctProp (iq: String, pq: String) = {
      r.getInstances(parseStringDLQuery(iq))
        .entities().iterator().asScala
        .foldLeft("")(
          (a, i) => a + "(" + replaceIndividualVariableName(i, pq) + ") or ")
        .dropRight(4)
    }

    def nominalProp(iq: String, pq: String) = {
      val inds = r.getInstances(parseStringDLQuery(iq))
        .entities().iterator().asScala
        .map(_.getIRI.getFragment)
      if (inds.isEmpty) ""
      else inds.foldLeft("{")((a,in) => a+in+",")
        .dropRight(1) + "}" + " and (" + pq + ")"
    }

    def buildProp(qbt: QueryBuildType, iq: String, pq: String) =
      qbt match {
        case QueryBuildType.DisjunctProp => disjunctProp(iq,pq)
        case QueryBuildType.NominalProp => nominalProp(iq,pq)
      }

    p match {
      case TFFproperty(_,_,qbt,iq,pq,_,_,_,_) => buildProp(qbt,iq,pq)
      case FTTproperty(_,_,qbt,iq,pq,_,_,_,_) => buildProp(qbt,iq,pq)
      case x => x.propQuery
    }

  }


  private[formal]
  def parseStringDLQuery(dlQuery: String): OWLClassExpression =
  {
    val importClosure: util.ArrayList[OWLOntology] =
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
      df.getOWLNothing
    else
      parser.parse(dlQuery)
  }

  private def replaceIndividualVariableName(i: OWLNamedIndividual, s: String): String
  = s.replaceAll("\\$\\{x\\}", i.getIRI.getFragment)

  private[formal]
  def hasRequiredResourceTypes(o: OWLOntology, p: Property) = {
    p.reqResTypes.get.toSet
        .subsetOf(
          o.imports().iterator().asScala.toSet.map(
            (x: OWLOntology) =>
              x.getOntologyID.getOntologyIRI.get().toString.split("#").head.split("/").last)
        )
  }

}
