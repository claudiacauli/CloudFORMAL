package com.cloud.formal.model

import java.io.File

import com.cloud.formal.FilePath
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
