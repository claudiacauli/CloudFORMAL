package com.claudiacauli.www.cloudformal.model

import java.io.File

import aws.cfn.FilePath
import org.semanticweb.owlapi.model.OWLOntologyManager

object ModelUtils {


  def loadActionsModelFromSpec(actPref: String, m: OWLOntologyManager): Unit = {
    m.loadOntologyFromOntologyDocument(
        new File(FilePath.ActionTerminology(actPref))
      )
  }


  def loadResourceSpecificationModelFromSpec
  (service: String, resource: String, m: OWLOntologyManager): Unit = {
    m.loadOntologyFromOntologyDocument(
      new File(FilePath.ResourceTerminology(service,resource))
    )
  }


}
