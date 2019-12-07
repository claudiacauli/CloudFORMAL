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

package com.cloud.formal.mapping.templates

import java.util.regex.Pattern

import argonaut.Json
import com.cloud.formal.{ModelType, Ontology}
import com.cloud.formal.mapping.{CFnType, Renaming, Specification}
import com.cloud.formal.model.{ModelIRI, ModelUtils}
import com.typesafe.scalalogging.LazyLogging
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.vocab.OWL2Datatype

import scala.jdk.OptionConverters._

private object Json2ResourceEncoder
{


  def subPropertiesNamesOfClassName (typeName: String, ssE: Json2StackSetEncoder,
                                     serviceType: String, resourceType: String,
                                     resourceOntology: OWLOntology): Set[String] = {



    def subPropertiesOfTypeName  = {
      val typeObj =
        Some(ssE.df
            .getOWLClass(
              ModelIRI.resourceTypeIRI(
                serviceType+resourceType,
                typeName)))
      subPropertiesOf(typeObj)
    }



    def allPropertiesInOntology =
      resourceOntology
        .dataPropertiesInSignature().toArray().toVector
        .asInstanceOf[Vector[OWLProperty]] ++
        resourceOntology
          .objectPropertiesInSignature().toArray().toVector
          .asInstanceOf[Vector[OWLProperty]]



    def propertyNameStartsWithTypeObjName(p: OWLProperty, typeObjName:String) =
      p match {
      case op:OWLObjectProperty
        => op.getIRI.toString
        .split(Ontology.Pound)
        .last.startsWith(typeObjName+Renaming.Delimiter)
      case dp:OWLDataProperty
        => dp.getIRI.toString
        .split(Ontology.Pound)
        .last.startsWith(typeObjName+Renaming.Delimiter)
    }


    def subPropertiesOf(typeObj: Option[OWLClass]) =
      typeObj match {
        case None     => Vector()
        case Some(c)  =>
          val typeObjName = c.getIRI.toString.split(Ontology.Pound).last
          allPropertiesInOntology
            .filter(p =>
              propertyNameStartsWithTypeObjName(p, typeObjName))
            .map {
              case dp: OWLDataProperty    => Left(dp)
              case op: OWLObjectProperty  => Right(op)
            }
      }



    subPropertiesOfTypeName.
      map {
        case Left(dp)   => dp.getIRI.toString.split(Ontology.Pound).last
        case Right(op)  => op.getIRI.toString.split(Ontology.Pound).last
      }.toSet
  }



  def expectedProperties(resourceType: String, ssE: Json2StackSetEncoder,
                         serviceType: String, resourceOntology: OWLOntology)
  : Set[String] =
    Json2ResourceEncoder
      .subPropertiesNamesOfClassName(
        resourceType.toLowerCase(),ssE,serviceType,
        resourceType,resourceOntology)



  def rangeNameOf(propertyName:String, resourceOntology: OWLOntology,
                  serviceType: String, resourceType: String,
                  ssE: Json2StackSetEncoder): Option[(String,String)] = {


    def rangeOf(propertyName:String) =
      if (isObjectProperty(propertyName))
        objectPropertyRange(propertyName) match {
        case None     => Some(CFnType.UnknownType)
        case Some(c)  => Some(Right(c))
      }
      else if (isDataProperty(propertyName))
        dataPropertyRange(propertyName) match {
        case None     => None
        case Some(dt) => Some(Left(dt))
      }
      else None


    def isObjectProperty(p: String)  =
      resourceOntology
        .containsObjectPropertyInSignature(
        ModelIRI.propertyTypeIRI(
          serviceType+resourceType, p)
        )


    def isDataProperty(p: String) =
      resourceOntology
        .containsDataPropertyInSignature(
        ModelIRI.propertyTypeIRI(
          serviceType+resourceType, p)
        )


    def dataPropertyRange(p: String)=
      resourceOntology
        .dataPropertyRangeAxioms(
          ssE.df
            .getOWLDataProperty(
              ModelIRI.propertyTypeIRI(serviceType+resourceType,p)))
        .findFirst().toScala match {
        case None     => None
        case Some(ax) =>
          Some(ax.datatypesInSignature().findFirst().get().getBuiltInDatatype)
      }


    def objectPropertyRange (p: String) =
      resourceOntology
        .objectPropertyRangeAxioms(
        resourceOntology.getOWLOntologyManager
          .getOWLDataFactory
          .getOWLObjectProperty(
            ModelIRI.propertyTypeIRI(serviceType+resourceType,p)))
        .findFirst().toScala match {
        case None => None
        case Some(ax) =>
          Some(ax.classesInSignature().findFirst().get())
      }




    rangeOf(propertyName) match {
      case Some(CFnType.UnknownType)
      => Some((CFnType.UnknownService, CFnType.UnknownResource))
      case Some(Left(dt:OWL2Datatype))
      => Some(("",ModelType.stringOf(dt)))
      case Some(Right(c:OWLClass))
      => Some((c.getIRI.toString.split(Ontology.Pound).head.split("/").last,
        c.getIRI.toString.split(Ontology.Pound).last))
      case _ => None
    }
  }


}


private class Json2ResourceEncoder( val tE:Json2TemplateEncoder,
                                    resourceLogicalId:String,
                                    val resourceJsonNode:Json)
extends LazyLogging
{

  val ssE: Json2StackSetEncoder      = tE.ssE
  val iE: Json2InfrastructureEncoder = ssE.iE
  val serviceType: String   = getServiceName
  val resourceType: String  = getResourceType
  val NodeEncoder     = new Json2NodeEncoder(this)
  var resource : StackSetResource = _





  def createResourceNodeWithAttributes: Map[String,StackSetResource] =
    if (tE.hasTrueCondition(resourceJsonNode))
    {
      resource = StackSetResource(
        resourceLogicalId, serviceType, resourceType,
        tE.ssE.stackSet, tE.template, attributesFromResourceJsonNode)

      importResourceSpecificationOntology()

      Map(resourceLogicalId -> resource)
    }
    else Map()



  def updateResourceName(): Unit =
    if (tE.hasTrueCondition(resourceJsonNode))
      resource.resourceName = getResourceName



  def deepInstantiationOfResource(): StackSetResource =
  {

    resource.givenProperties =
      givenProperties.flatMap(propName =>
        Map(propName -> nodeObjectForProperty(
          propName, serviceType,resourceType,
          ssE, resourceOntology)))
        .toMap ++
      defaultProperties

    resource.absentProperties =
      Json2ResourceEncoder.expectedProperties(
          resourceType, ssE, serviceType, resourceOntology) --
      resource.givenProperties.keySet.map(_.toLowerCase)

    resource
  }



  def assignedDefaultNode(value: Any) =
    value match {
      case b: Boolean => BooleanNode(b)
      case s: String  => StringNode(s)
      case i: Integer => IntNode(i)
        // TODO Extend with more cases!
      //  Now my DefaultsMap Only contains bools, strings, and int so no problem
    }


  def defaultProperties =
    (Json2ResourceEncoder
      .expectedProperties(
        resourceType, ssE, serviceType, resourceOntology) --
      givenProperties.map(_.toLowerCase))
      .flatMap( absProp =>
        DefaultsMap.lookUp(serviceType, absProp) match {
          case None     => Map()
          case Some(v)  => Map(absProp -> assignedDefaultNode(v))
        }
      ).toMap




  def resourceOntology: OWLOntology =
    ssE.manager
      .getOntology(
        ModelIRI.resourceTerminologyIRI(
          serviceType+resourceType))





  private def givenProperties =
    resourceJsonNode
      .field(Specification.Properties)
      .getOrElse(Json.jEmptyObject)
      .objectFieldsOrEmpty.map(f =>
      resourceType + Renaming.Delimiter +f.toString
    ).toSet


  private def nodeObjectForProperty(propFullName: String, serviceType: String,
                                    resourceType:String, ssE :Json2StackSetEncoder,
                                    resourceOntology: OWLOntology) =
  {
    val propTemplateName  = propFullName
      .split(resourceType+ Renaming.Delimiter).last

    val propContentNode   = resourceJsonNode
      .field(Specification.Properties).get
      .field(propTemplateName).get

    val propContentType   =
      Json2ResourceEncoder
        .rangeNameOf(propFullName,resourceOntology,
          serviceType,resourceType,ssE)

    NodeEncoder.encode(propContentNode,propContentType)

  }


  private def isCustomResource =
    Pattern
      .compile(Specification.CustomResourceRegex,Pattern.CASE_INSENSITIVE)
      .matcher(resourceJsonNode.field(Specification.Type).get.string.get)
      .matches()



  private def getServiceName: String =
    if (isCustomResource)
      Specification.CustomServiceType
    else
      resourceJsonNode
        .field(Specification.Type).get
        .string.get.split(Specification.TypeDelimiter)(1)


  private def getResourceType: String =
    if (isCustomResource)
      Specification.CustomResourceType
    else
      resourceJsonNode
        .field(Specification.Type).get
        .string.get.split(Specification.TypeDelimiter)(2)



  private def getResourceName: String =
    ResourcesNameFieldsMap
      .lookUp(serviceType,resourceType) match {
      case None     => resourceLogicalId
      case Some(f)  =>
        if (resourceJsonNode.hasField(Specification.Properties) &&
          resourceJsonNode.field(Specification.Properties).get.hasField(f))
          NodeEncoder.encode(
            resourceJsonNode.field(Specification.Properties).get
              .field(f).get ) match {
            case StringNode(s) => s
            case _ => resourceLogicalId
          }
        else
          resourceLogicalId
    }



  private def valueNodeFromSingleAttribute(attrNode:Json) : GenericValueNode =
    if (attrNode.isString)
      StringNode(attrNode.string.get)
    else if (attrNode.isNumber)
      LongNode(attrNode.number.get.truncateToLong)
    else if (attrNode.isBool)
      BooleanNode(attrNode.bool.get)
    else if (attrNode.isArray)
      ListNode(
        attrNode.array.get
          .map(valueNodeFromSingleAttribute)
          .toVector)
    else NoValue



  private def attributesFromResourceJsonNode: Map[String,GenericValueNode]  =
    resourceJsonNode
      .field(Specification.Attributes) match {
      case None => Map()
      case Some(attributesNode) =>
        attributesNode
          .objectFieldsOrEmpty
          .flatMap (f =>
            Map(f.toString.toLowerCase ->
              valueNodeFromSingleAttribute(attributesNode.field(f).get)))
          .toMap
    }



  private def importResourceSpecificationOntology() : Unit  =
  {
    ModelUtils
      .loadResourceSpecificationModelFromSpec(
        serviceType,resourceType,ssE.manager)

  }



}
