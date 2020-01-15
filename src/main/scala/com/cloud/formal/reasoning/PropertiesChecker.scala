package com.cloud.formal.reasoning

import org.semanticweb.owlapi.model.{OWLDataFactory, OWLOntology, OWLOntologyManager}
import org.semanticweb.owlapi.reasoner.InconsistentOntologyException

class PropertiesChecker (name: String, o: OWLOntology, df: OWLDataFactory, m: OWLOntologyManager) {

  private val pG = new PropertiesGenerator
  private val r = Reasoner.create(o, df, m)
  private val pE = new PropertyEvaluator(r,o,df,m)

  def run() = {
    pG.init()
    try {
      r.classify()
      pG.propMap.values.foreach(pE.evalAndPrint)
    } catch {
      case e: InconsistentOntologyException
        => println("  INCONSISTENT ONTOLOGY FOUND. SKIPPING. ")
    }
  }

}
