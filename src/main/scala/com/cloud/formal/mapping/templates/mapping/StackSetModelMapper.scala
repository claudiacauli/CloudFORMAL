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

package com.cloud.formal.mapping.templates.mapping

import com.cloud.formal.AwsOntology
import com.cloud.formal.mapping.templates._
import com.cloud.formal.model.ModelIRI
import com.typesafe.scalalogging.LazyLogging
import org.semanticweb.owlapi.model._

import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._


protected object StackSetModelMapper
{

  def encode(stackSet: StackSet, infrastructure: Infrastructure): StackSetModel =
    new StackSetModelMapper(stackSet, infrastructure)
      .encode()

}




private class StackSetModelMapper(stackSet: StackSet, infrastructure: Infrastructure)
extends LazyLogging
{

  val m: StackSetModel =
    new StackSetModel(
      stackSet.name,
      stackSet.manager
        .ontologies().toArray().toVector
        .flatMap( o =>
          Set(o.asInstanceOf[OWLOntology])))



  def encode(): StackSetModel =
  {

    stackSet.templates
      .foreach(
        initializeStackIndividualFromTemplate)

    stackSet.templates
      .flatMap(t => t.resources.toVector
          .map(r => (t,r)))
      .foreach(
        initializeResourceIndividual)

    infrastructure
      .externalResources
        .foreach(initializeExternalResource)

    stackSet.templates
      .flatMap(_.resources.toVector)
        .foreach( r =>
          m.ontology.add(encodeResource(r._2).asJava))

    m
  }


  private def initializeStackIndividualFromTemplate(t: Template) =
  {
//    val stackIndividualName =
//      stackSet.name + t.name + AwsOntology.StackSuffix

    val stackIndividual =
      m.df.getOWLNamedIndividual(ModelIRI
        .resourceInstanceIRI(stackSet.name, t.name, AwsOntology.StackSuffix))

    m.ontology.add(
      m.df.getOWLClassAssertionAxiom(
        classFromIRI(ModelIRI
          .awsConceptIRI(AwsOntology.StackSuffix)),
        stackIndividual
      ))

    m.ontology.add(
      m.df.getOWLObjectPropertyAssertionAxiom(
        m.df.getOWLObjectProperty(ModelIRI
          .awsPropertyIRI(AwsOntology.IsDeployedIn)),
        stackIndividual,
        m.df.getOWLNamedIndividual(ModelIRI
          .awsAccountIRI(t.parameters(
            PseudoParameter.AccountId)
            .asInstanceOf[StringNode].value)))
    )
  }


  private def initializeResourceIndividual(o: (Template,(String,StackSetResource)) ) =
  {
    m.ontology.add(m.df.getOWLClassAssertionAxiom(
      classFromIRI(
        ModelIRI.resourceTypeIRI(
          o._2._2.serviceType + o._2._2.resourceType,
          o._2._2.resourceType)),
      getResourceIndividual(o._2._2)
    ))

    m.ontology.add(m.df.getOWLObjectPropertyAssertionAxiom(
      m.df.getOWLObjectProperty(ModelIRI
        .awsPropertyIRI(AwsOntology.IsInStack)),
      getResourceIndividual(o._2._2),
      m.df.getOWLNamedIndividual(ModelIRI
        .resourceInstanceIRI(stackSet.name,o._1.name,AwsOntology.StackSuffix)
    )))
  }


  private def initializeExternalResource(eR: ExternalResource) =
    m.ontology.add(
      m.df.getOWLClassAssertionAxiom(
        classFromIRI(ModelIRI
          .awsConceptIRI(AwsOntology.ExternalResource)),
        m.df.getOWLNamedIndividual(ModelIRI
          .externalEntityIRI(infrastructure.name,eR.name))
      ))


  private def getResourceIndividual(res: StackSetResource) = {
    val newResource = m.df
      .getOWLNamedIndividual(ModelIRI
        .resourceInstanceIRI(stackSet.name,
          res.template.name, res.resourceLogicalId))
    addComment(newResource, AwsOntology.ResourceAnnotationComment)
    newResource
  }



  private def encodeResource(r: StackSetResource)  =
  {
    val resourceInstanceIRI =
      ModelIRI.resourceInstanceIRI(
        stackSet.name,r.template.name,r.resourceLogicalId)

    encodeAllGivenSubproperties(resourceInstanceIRI,r.givenProperties,r) ++
      encodeAllAbsentSuproperties(resourceInstanceIRI,r.absentProperties,r)
  }


  private def encodeAllGivenSubproperties(source: IRI, givenProp: Map[String, Node],
                                          res: StackSetResource): Vector[OWLAxiom] =
    givenProp.toVector
      .flatMap(p =>
        encodeProperty(m.df
          .getOWLNamedIndividual(source), p._1, p._2,res))


  private def encodeAllAbsentSuproperties(source: IRI, absentProp: Set[String],
                                          res: StackSetResource): Vector[OWLAxiom]=
    absentProp.toVector
      .flatMap(p =>
        absentPropertyAxiom(m.df
          .getOWLNamedIndividual(source), p,res))


  private def encodeProperty(source: OWLIndividual, prop: String, target: Node, res: StackSetResource)=

    oProperty(prop,res) match {

      case Some(op) =>
        target match {
          case null => Vector() // This should not be here. Try to get rid of null case. Check where do you assign it.
          case ListNode(vec) =>
            arrayCardinalityAxiom(source,op,vec.size) ++
            vec.flatMap{
              case StringNode(s) =>
                val name = "ext_"+s.replaceAll("\"","")
                encodeObjectProperty(source, op, ExternalResource(name,infrastructure),res)
              case NoValue => Vector()
              case n => encodeObjectProperty(source, op, n.asInstanceOf[ObjectNode],res)
            }
          case MapNode(map)
          => map.toVector
            .flatMap(e => encodeObjectMapEntry(source, op, e))
          case NoValue
          => Vector()
          case _
          => encodeObjectProperty(source, op,
            target match {
              case StringNode(s) =>
                    val nam = "ext_"+s.replaceAll("\"","")
                    ExternalResource(nam,infrastructure)
              case _             => target.asInstanceOf[ObjectNode]
            },res)
        }

      case None =>
        dProperty(prop,res) match {
          case Some(dp) => target match {
            case ListNode(vec) if vec.head.isInstanceOf[GenericValueNode]
            => vec.flatMap(n =>
              encodeValueProperty(source,dp,n.asInstanceOf[GenericValueNode]))
            case ListNode(vec) if vec.head.isInstanceOf[ObjectNode]
            => vec.flatMap(n =>
              encodeValueProperty(source,dp,StringNode(n.toString)))
            case MapNode(map)
            => map.toVector.flatMap (e =>
              encodeValueMapEntry(source,dp,e.asInstanceOf[(String,GenericValueNode)]))
            case NoValue
            => Vector()
            case StackSetResource(_,s,r,_,_,_)
            =>
              logger.error("Data Property " + dp.toString.split(AwsOntology.Pound).last +
                " of " + res.serviceType + res.resourceType + " points instead to r of type " +
                s + r + AwsOntology.Pound + r)
              Vector()
            case _ =>
              encodeValueProperty(source, dp, target.asInstanceOf[GenericValueNode])
          }
          case None =>
            if (res.serviceType == TemplateTag.CloudFormation &&
              res.resourceType == TemplateTag.CustomResource)
              target match {
                case n: GenericValueNode
                => encodeValueProperty(
                    source,
                    m.df.getOWLDataProperty(ModelIRI
                      .propertyTypeIRI(res.serviceType+res.resourceType,prop)),
                      n)
                case n: ObjectNode
                  => encodeObjectProperty(
                      source,
                      m.df.getOWLObjectProperty(ModelIRI
                        .propertyTypeIRI(res.serviceType+res.resourceType,prop)),
                      n,res)
              }
            else {
              logger.error("Property " + prop + " of " + res.serviceType + res.resourceType
                + "("+ source.asOWLNamedIndividual().getIRI + ") is not found either as " +
                "object or as data property! ")
              Vector()
            }
        }

    }


  private def encodeValueMapEntry(source:OWLIndividual,
                                  dp:OWLDataProperty,
                                  entry:(String,GenericValueNode) ) =
    Vector() // TODO


  private def encodeObjectMapEntry(source: OWLIndividual,
                                   op:OWLObjectProperty,
                                   entry:(String,Node)) =
    Vector() // TODO



  private def absentPropertyAxiom(source: OWLIndividual,
                                   propName: String,
                                   res: StackSetResource)= {
    Vector(m.df
      .getOWLSubClassOfAxiom(m.df
        .getOWLObjectOneOf(source),
        oProperty(propName,res) match {
          case None     => m.df
            .getOWLObjectComplementOf(m.df
              .getOWLDataSomeValuesFrom(dProperty(propName,res).get, m.df.getTopDatatype))
          case Some(op) => m.df
            .getOWLObjectAllValuesFrom(op, m.df.getOWLNothing)
        }
      ))
  }





  private def arrayCardinalityAxiom(source: OWLIndividual,
                                          p: OWLProperty,
                                          k: Int): Vector[OWLSubClassOfAxiom] =
    p match {
      case op: OWLObjectProperty =>
        Vector(m.df.getOWLSubClassOfAxiom(
          m.df.getOWLObjectOneOf(source),
          m.df.getOWLObjectMaxCardinality(k,op)))
      case dp: OWLDataProperty =>
        Vector(m.df.getOWLSubClassOfAxiom(
          m.df.getOWLObjectOneOf(source),
          m.df.getOWLDataMaxCardinality(k,dp)))
    }





  private def encodeValueProperty(source: OWLIndividual,
                                  dp: OWLDataProperty,
                                  value: GenericValueNode): Vector[OWLAxiom] =
    value match {
      case NoValue        => Vector()
      case ListNode(vec:Vector[Node]) =>
        vec.flatMap(n =>
          encodeValueProperty(source,dp,n.asInstanceOf[GenericValueNode])) ++
          arrayCardinalityAxiom(source,dp,vec.size)
      case MapNode(map) =>
        map.toVector.flatMap(entry =>
          encodeValueMapEntry(source,dp,entry.asInstanceOf[(String,GenericValueNode)]))
      case StringNode(v)  => Vector(m.df.getOWLDataPropertyAssertionAxiom(dp,source,v))
      case IntNode(v)     => Vector(m.df.getOWLDataPropertyAssertionAxiom(dp, source, v))
      case BooleanNode(v) => Vector(m.df.getOWLDataPropertyAssertionAxiom(dp, source, v))
      case LongNode(v)    => Vector(m.df.getOWLDataPropertyAssertionAxiom(dp, source, v.toInt))
      case FloatNode(v)   => Vector(m.df.getOWLDataPropertyAssertionAxiom(dp, source, v))
      case DoubleNode(v)  => Vector(m.df.getOWLDataPropertyAssertionAxiom(dp, source,v))
      case TimeStampNode(v)           => Vector(m.df.getOWLDataPropertyAssertionAxiom(dp, source, v))
      case CommaDelimitedListNode(v)  => Vector(m.df.getOWLDataPropertyAssertionAxiom(dp, source, v))
      case JsonNode(v)                => Vector(m.df.getOWLDataPropertyAssertionAxiom(dp, source, v))
    }


  private def encodeObjectProperty(source: OWLIndividual,
                                   op: OWLObjectProperty,
                                   target: ObjectNode,
                                    res: StackSetResource) =
  {

    if (target==null)
    println(s"Property $op from ${res.serviceType}+${res.resourceType} " +
      s"is assigned to individual $source but points to an objectNode NULL.")


    target match {
      case StackSetResource(id,_,_,ss,t,_)
      => Vector(m.df
        .getOWLObjectPropertyAssertionAxiom(op, source,
          resourceIndividualFromStackSet(id,ss,t)))

      case Subproperty(gP, aP)
      => val individualRandomIRI =
        ModelIRI.subpropertyBlankNodeIRI(stackSet.name)

        createBlankNode(source,op, individualRandomIRI,res) ++
          encodeAllGivenSubproperties(individualRandomIRI, gP,res) ++
          aP.flatMap(p
          => absentPropertyAxiom(m.df
              .getOWLNamedIndividual(individualRandomIRI), p,res))

      case ExternalResource(v,infr)
      => Vector(m.df.getOWLObjectPropertyAssertionAxiom(
        op, source, externalResourceIndividualFromInfrastructure(v,infr)))

//      case ListOfResources(v) =>
//        v.head match {
//          case StackSetResource(id, _, _, ss, t, _) =>
//            Vector(m.df.getOWLObjectPropertyAssertionAxiom(op, source, resourceIndividualFromStackSet(id,ss,t)))
//        }

      case x => logger.warn(s"Attempting to encode Object Property " +
        op.getIRI.toString + " " +
        s"with unexpected target type. Should be one of " +
        s"StackSetResource, Subproperty, or ExternalResource.")
        Vector()
    }
  }



  private def createBlankNode(sourceIndiv: OWLIndividual,
                              op: OWLObjectProperty,
                              uniqueRandomIRI: IRI,
                              res: StackSetResource)=
  {
    val newNode = m.df.getOWLNamedIndividual(uniqueRandomIRI)
    addComment(newNode, AwsOntology.SubpropertyAnnotationComment)

    Vector(m.df.getOWLObjectPropertyAssertionAxiom(op, sourceIndiv, newNode)) ++
      (range(op,res) match {
        case None     => Vector(m.df.getOWLDeclarationAxiom(newNode))
        case Some(c)  => Vector(m.df.getOWLClassAssertionAxiom(c, newNode))
      })
  }


  private def resourceIndividualFromStackSet(name:String,
                                             stackSet: StackSet,
                                             template: Template) = {
    m.df.getOWLNamedIndividual(ModelIRI
      .resourceInstanceIRI(stackSet.name,template.name,name))
  }



  private def externalResourceIndividualFromInfrastructure
                                    (name:String, infr: Infrastructure) =
    m.df.getOWLNamedIndividual(ModelIRI
      .externalEntityIRI(infr.name,name))



  private def range(op: OWLObjectProperty, res: StackSetResource)=
    m.manager.getOntology(ModelIRI
      .resourceTerminologyIRI(
        res.serviceType+res.resourceType))
      .objectPropertyRangeAxioms(op)
      .findFirst().toScala match {
      case Some(ax) => Some(ax.getRange.asOWLClass())
      case None     => None
    }


  private def modelContainsObjProperty(name: String,
                                       res: StackSetResource) =
    m.manager.getOntology(ModelIRI
      .resourceTerminologyIRI
      (res.serviceType+res.resourceType))
      .containsObjectPropertyInSignature(ModelIRI
        .propertyTypeIRI(res.serviceType+res.resourceType,name))


  private def oProperty(name:String, res:StackSetResource) =
    if (modelContainsObjProperty(name,res))
      Some(m.df
        .getOWLObjectProperty(
          ModelIRI.propertyTypeIRI(
            res.serviceType+res.resourceType,name)))
    else
      None


  private def modelContainsDataProperty(name: String,
                                        res: StackSetResource) =
    m.manager.getOntology(ModelIRI
      .resourceTerminologyIRI
      (res.serviceType+res.resourceType))
      .containsDataPropertyInSignature(ModelIRI
        .propertyTypeIRI(res.serviceType+res.resourceType,name))


  private def dProperty(name:String, res:StackSetResource)=
    if (modelContainsDataProperty(name,res))
      Some(m.df
        .getOWLDataProperty(
          ModelIRI.propertyTypeIRI(
            res.serviceType+res.resourceType,name)))
    else
      None


  private def classFromIRI(iri:IRI) =
    m.df.getOWLClass(iri)


  private def addComment(i:OWLNamedIndividual, comm:String) = {
    val commentAxiom  =
      m.df.getOWLAnnotationAssertionAxiom(
        i.getIRI,
        m.df.getRDFSComment(comm))
    m.manager.applyChange(
      new AddAxiom(m.ontology,
        commentAxiom))
  }


}

