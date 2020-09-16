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

import com.cloud.formal.{Ontology, OntologySuffix}
import org.semanticweb.owlapi.model.{IRI, OWLClassExpression, OWLNamedObject, OWLPropertyExpression}

object IRIUtils {

  val AWS = "aws"

  def isStackSetObject (i : OWLNamedObject) : Boolean =
    objIriEndsWith(i,"stackset")

  def isInfrastructureObject (i : OWLNamedObject) : Boolean =
    objIriEndsWith(i,"infrastructure")

  def isStackSetOrInfrastructureObject (i: OWLNamedObject) : Boolean =
    isStackSetObject(i) || isInfrastructureObject(i)

  def isAwsOntologyObject (i : OWLNamedObject) : Boolean =
    objIriEndsWith(i,AWS)

  def isSpecPredicate (pred: OWLNamedObject) : Boolean =
    !objIriEndsWith(pred,AWS) &&
      !objIriEndsWith(pred,OntologySuffix.Infrastructure) &&
      !objIriEndsWith(pred,OntologySuffix.StackSet)

  def isSpecConcept(c: OWLClassExpression) : Boolean =
    !isThing(c) && !isNothing(c) &&
      c.isClassExpressionLiteral && isSpecPredicate(c.asOWLClass())

  def isSpecRole(r: OWLPropertyExpression) : Boolean =
    !r.isOWLBottomDataProperty && !r.isOWLTopDataProperty &&
      !r.isOWLBottomObjectProperty && !r.isOWLTopObjectProperty &&
      ((r.isDataPropertyExpression && isSpecPredicate(r.asDataPropertyExpression().asOWLDataProperty()))
        ||
        (r.isDataPropertyExpression && isSpecPredicate(r.asDataPropertyExpression().asOWLDataProperty())))

  def isThing (ce: OWLClassExpression) : Boolean =
    ce.isOWLThing

  def isNothing (ce: OWLClassExpression) : Boolean =
    ce.isOWLNothing

  def isDFDIRI(iri: IRI): Boolean =
    iri.toString.split(Ontology.Pound).head.endsWith("/dfd")

  def isServiceDFDIRI(iri:IRI): Boolean =
    iri.toString.split(Ontology.Pound).head.endsWith("-dfd")

  def iriEndsWith (iri: IRI, s: String) : Boolean =
    iri.toString.split(Ontology.Pound).head.endsWith(s)

  def objIriEndsWith (obj : OWLNamedObject, s: String) : Boolean =
    iriEndsWith(obj.getIRI,s)

  def getSimpleName (obj: OWLNamedObject) : String =
    obj.getIRI.toString.split(Ontology.Pound)(1)

}
