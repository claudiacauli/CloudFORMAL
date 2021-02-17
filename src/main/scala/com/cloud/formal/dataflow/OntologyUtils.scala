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

package com.cloud.formal.dataflow

import java.util

import com.cloud.formal.Ontology
import com.cloud.formal.binding.Interface
import com.cloud.formal.dataflow.{LabelsUtils => LU}
import com.cloud.formal.reasoning.ReasonerWrapper
import org.semanticweb.owlapi
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{IRI, OWLAxiom, OWLClassAssertionAxiom, OWLDataFactory, OWLDataPropertyAssertionAxiom, OWLIndividual, OWLIndividualAxiom, OWLNamedIndividual, OWLObjectPropertyAssertionAxiom, OWLOntology, OWLOntologyManager}
import org.semanticweb.owlapi.reasoner.OWLReasoner
import org.semanticweb.owlapi.util.{InferredAxiomGenerator, InferredClassAssertionAxiomGenerator, InferredDataPropertyCharacteristicAxiomGenerator, InferredDisjointClassesAxiomGenerator, InferredEquivalentClassAxiomGenerator, InferredEquivalentDataPropertiesAxiomGenerator, InferredEquivalentObjectPropertyAxiomGenerator, InferredIndividualAxiomGenerator, InferredInverseObjectPropertiesAxiomGenerator, InferredObjectPropertyCharacteristicAxiomGenerator, InferredOntologyGenerator, InferredPropertyAssertionGenerator, InferredSubClassAxiomGenerator, InferredSubDataPropertyAxiomGenerator}

import scala.jdk.StreamConverters._


object OntologyUtils {

  val SubpropertyString = "subproperty"


  def loadInfrastructureModel(inPath: String)
  : (OWLOntology, OWLDataFactory, OWLOntologyManager, String) =
    Interface.loadModel(inPath,false)

  def startReasoner(o: OWLOntology, df: OWLDataFactory, m: OWLOntologyManager)
  : OWLReasoner =
  {
    val r = ReasonerWrapper.create(o,df,m)
    r.computeAllInferences(r.reasonerEngine)
    r.reasonerEngine
  }



  def getAllObjectPropertyAssertions(m: OWLOntologyManager)
  : Set[OWLObjectPropertyAssertionAxiom] =
    m.ontologies().toScala(Set) flatMap getAllObjPropAssertionsOnto

  def getAllDataPropertyAssertions(m: OWLOntologyManager)
  : Set[OWLDataPropertyAssertionAxiom] =
    m.ontologies().toScala(Set) flatMap getAllDataPropAssertionsOnto

  def getAllClassAssertions(m: OWLOntologyManager)
  : Set[OWLClassAssertionAxiom] =
    m.ontologies().toScala(Set) flatMap getAllClassAssertionsOnto

  // Does a node ONLY have data properties?
  def isDataRecordNode(o: OWLOntology, n: OWLNamedIndividual): Boolean =
    getAllObjPropAssertionsIndividual(o,n).isEmpty &&
      getAllDataPropAssertionsIndividual(o,n).nonEmpty

  def hasRecords (o: OWLOntology, n: OWLNamedIndividual): Boolean =
    getAllDataPropAssertionsIndividual(o,n).nonEmpty

  def getAllSubPropertyNodes(m: OWLOntologyManager): List[OWLNamedIndividual] =
    m.ontologies().toScala(List)
      .flatMap(o => o.individualsInSignature().toScala(List)
        .filter(i => IRIUtils.getSimpleName(i).startsWith(SubpropertyString)
        ))

  private[dataflow]
  def getAllAccounts(m: OWLOntologyManager, df: OWLDataFactory): List[OWLNamedIndividual] =
    m.ontologies().toScala(List)
      .flatMap( _.classAssertionAxioms(
        df.getOWLClass(IRI.create(Ontology.VersionStringIRI + "aws#awsaccount"))).toScala(List)
        .map(_.getIndividual.asOWLNamedIndividual()))

  private[dataflow]
  def getAllResourceNodes(m: OWLOntologyManager) =
  {
    m.ontologies().toScala(Set)
      .flatMap(_.individualsInSignature().toScala(Set)
        //.filter (IRIUtils.isStackSetOrInfrastructureObject)
        .filter (i =>
          !IRIUtils.getSimpleName(i).startsWith(LU.SubpropertyString)
            && !IRIUtils.objIriEndsWith(i,IRIUtils.AWS)))
  }


  def getAllDataRecordOnlyNodes(m: OWLOntologyManager): List[OWLNamedIndividual] =
    m.ontologies().toScala(List)
      .flatMap(o => {
        o.individualsInSignature().toScala(List)
          .filter(i => isDataRecordNode(o,i))
      })

  def getAllSomeDataRecordNodes(m: OWLOntologyManager): List[OWLNamedIndividual] =
    m.ontologies().toScala(List)
      .flatMap(o => {
        o.individualsInSignature().toScala(List)
          .filter(i => hasRecords(o,i))
      })


  private[dataflow]
  def getAllTerminalAsString(m: OWLOntologyManager): Set[String] =
    OntologyUtils
      .getAllDataPropertyAssertions(m)
        .map(dpa => dpa.getObject.toString.split("\\^\\^")(0))






  private[dataflow]
  def addInferredAxiomsToMainOntologyManager(o: OWLOntology, m: OWLOntologyManager,reasEngine: OWLReasoner) =
    m.addAxioms(o,getInferredOntology(reasEngine).axioms())


  private[dataflow]
  def getInferredOntology(reasEngine: OWLReasoner) =
  {

    val gens = new util.ArrayList[InferredAxiomGenerator[_ <: OWLAxiom]]
      gens.add(new InferredSubClassAxiomGenerator)
      gens.add(new InferredClassAssertionAxiomGenerator)
      gens.add(new InferredDataPropertyCharacteristicAxiomGenerator)
      gens.add(new InferredEquivalentClassAxiomGenerator)
      gens.add(new InferredEquivalentDataPropertiesAxiomGenerator)
      gens.add(new InferredEquivalentObjectPropertyAxiomGenerator)
      gens.add(new InferredInverseObjectPropertiesAxiomGenerator)
      gens.add(new InferredObjectPropertyCharacteristicAxiomGenerator)
      gens.add(new InferredPropertyAssertionGenerator)
      gens.add(new owlapi.util.InferredSubClassAxiomGenerator)
      gens.add(new InferredSubDataPropertyAxiomGenerator)
      gens.add(new owlapi.util.InferredSubObjectPropertyAxiomGenerator)
      gens.addAll(new util.ArrayList[InferredIndividualAxiomGenerator[_ <: OWLIndividualAxiom]])
      gens.add(new InferredDisjointClassesAxiomGenerator)

    val inferManager = OWLManager.createOWLOntologyManager()
    val inferOntology = inferManager.createOntology()
    //println("About to fill the ontology")
    new InferredOntologyGenerator(reasEngine,gens)
      .fillOntology(inferManager.getOWLDataFactory,inferOntology)
    //println("Ontology filled")
    inferOntology

  }



  private
  def getAllObjPropAssertionsOnto (o: OWLOntology)
  : Set[OWLObjectPropertyAssertionAxiom] =
    o.individualsInSignature().toScala(Set)
      .flatMap ( i => getAllObjPropAssertionsIndividual(o,i) )


  // TODO Check if this needs to be for the entire package or not...
  private[dataflow]
  def getAllObjPropAssertionsIndividual (o: OWLOntology, i: OWLIndividual)
  : Set[OWLObjectPropertyAssertionAxiom] =
    o.objectPropertyAssertionAxioms(i).toScala(Set)



  private
  def getAllDataPropAssertionsOnto (o: OWLOntology)
  : Set[OWLDataPropertyAssertionAxiom] =
    o.individualsInSignature().toScala(Set)
      .flatMap( i => getAllDataPropAssertionsIndividual(o,i) )




  def getAllDataPropAssertionsIndividual (o: OWLOntology, i: OWLIndividual)
  : Set[OWLDataPropertyAssertionAxiom] =
    o.dataPropertyAssertionAxioms(i).toScala(Set)



  private
  def getAllClassAssertionsOnto (o: OWLOntology)
  : Set[OWLClassAssertionAxiom] =
    o.individualsInSignature().toScala(Set)
      .flatMap ( i => getAllClassAssertionsIndividual(o,i) )


  // TODO Check if this needs to be for the entire package or not...
  private[dataflow]
  def getAllClassAssertionsIndividual(o: OWLOntology, i: OWLIndividual)
  : Set[OWLClassAssertionAxiom] =
    o.classAssertionAxioms(i).toScala(Set)


}
