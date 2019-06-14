package com.claudiacauli.www.cloudformal.mapping.templates

import java.io.File

import aws.cfn.FilePath
import org.semanticweb.owlapi.model.OWLOntologyManager

private class StackSet( val name:String,
                val infrastructure: Infrastructure,
                val manager: OWLOntologyManager) {

  private[templates] var templates: Vector[Template] = Vector()
  private[templates] var foreignNodes: Map[String,ExternalResource] = Map()

  manager
    .loadOntologyFromOntologyDocument(new File(FilePath.AwsOntology))

  override def toString: String = {
    "\tStackSet: " + name + ", includes Templates: " + "\n" +
      templates.foldLeft("")((a,b)=> a + b.toString + "\n")
  }

}
