package com.cloud.formal.mapping.templates.mapping

import java.io.File

import com.cloud.formal.mapping.templates.Infrastructure
import com.cloud.formal.model.{Model, ModelIRI, ModelWriter}
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{OWLDataFactory, OWLOntology, OWLOntologyManager}


object PermissionsModel
{
  def fromInfrastructure(infrastructure: Infrastructure): Model =
    PermissionsModelMapper
      .encode(infrastructure)

}


private class PermissionsModel(val name: String)
  extends Model
{

  val manager :OWLOntologyManager
  = OWLManager.createOWLOntologyManager()
  val ontology: OWLOntology
  = manager.createOntology(ModelIRI.permissionsModelIRI(name))
  val df: OWLDataFactory
  = manager.getOWLDataFactory


  def writeToOutputFolder(destinationFolder: String): Unit =
  {
    val dir = new File(destinationFolder+"/"+name)
    stackSetFilesInSubdirs(dir)
      .foreach(f =>
        importFile(this,f))
    ModelWriter
      .writePermissionToOutputFolder(
        this,destinationFolder)
  }




}
