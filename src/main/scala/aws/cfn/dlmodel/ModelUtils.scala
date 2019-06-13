package aws.cfn.dlmodel

import java.io.File

import org.semanticweb.owlapi.model.OWLOntologyManager

object ModelUtils {


  def loadActionsModelFromSpec(actPref: String, m: OWLOntologyManager): Unit = {
    m.loadOntologyFromOntologyDocument(
        new File("src/main/resources/terminology/actions/" +
          actPref + OntologySuffix.Actions + Extension.Owl)
      )
  }


  def loadResourceSpecificationModelFromSpec
  (service: String, resource: String, m: OWLOntologyManager): Unit = {
    m.loadOntologyFromOntologyDocument(
      new File("src/main/resources/terminology/resourcespecificationsOwl/" +
        service + resource + Extension.Owl)
    )
  }


}
