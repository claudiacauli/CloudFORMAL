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

package com.cloud.formal.mapping.specifications

import com.cloud.formal.model.ModelIRI
import com.cloud.formal.{ModelType => Type}
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.vocab.OWL2Datatype

import scala.jdk.CollectionConverters._


private object ResourceSpecificationModelMapper
{

  private[specifications] 
  def fromSpecification(resourceSpecification: ResourceSpecification) =
    new ResourceSpecificationModelMapper(resourceSpecification)
      .encode()

}


private class ResourceSpecificationModelMapper(resSpec : ResourceSpecification)
{
  
  private val m : ResourceSpecificationModel = new ResourceSpecificationModel(resSpec.name)

  private def encode() = {
    m.ontology.add(
      (owlClassFromResourceType(resSpec.resource) ++
        (resSpec.subproperties flatMap owlClassFromSubpropertyType))
        .asJava)
    m
  }

  
  private def owlClassFromResourceType(resourceType: ResourceValueType) = {
    
    addResourceTypeToOntology(resourceType.name)
    
    (resourceType.attributes flatMap owlPropertyFromGenericAttributeType) ++
      (resourceType.properties flatMap owlPropertyFromGenericPropertyType)
  
  }


  
  private def owlClassFromSubpropertyType(subpropertyType: SubpropertyValueType) = {
    
    addSubpropertyTypeToOntology(subpropertyType.name)
    
    subpropertyType.properties flatMap owlPropertyFromGenericPropertyType
  
  }

  

  
  private def owlPropertyFromGenericPropertyType(propertyType: GenericPropertyType) =
    propertyType match {
      case ListOfPrimitiveProperty(pT,n,d,r)      => axioms(n,d.name,Type.fromString(pT),r,f = false)
      case ListOfNonPrimitiveProperty(nPT,n,d,r)  => axioms(n,d.name,nPT,r,f =false)
      case MapOfPrimitiveProperty(pT,n,d,r)       => mapAxioms(n,d.name,pT,r,f = false,isDT = true)
      case MapOfNonPrimitiveProperty(nPT,n,d,r)   => mapAxioms(n,d.name,nPT,r,f = false)
      case SubpropertyProperty(ran,prop,d,r)      => axioms(prop,d.name,ran,r,f = true)
      case ResourceProperty(otherResSpec,resT,n,d,r)          => axioms(n,d.name,resT,r,f = true, otherResSpec)
      case ListOfResourcesProperty(otherResSpec,restT,n,d,r)  => axioms(n,d.name,restT,r,f =false, otherResSpec)
      case IntProperty(n,d,r)       => axioms(n,d.name,Type.OwlInt,r,f = true)
      case FloatProperty(n,d,r)     => axioms(n,d.name,Type.OwlFloat,r,f = true)
      case DoubleProperty(n,d,r)    => axioms(n,d.name,Type.OwlDouble,r,f = true)
      case LongProperty(n,d,r)      => axioms(n,d.name,Type.OwlLong,r,f = true)
      case BooleanProperty(n,d,r)   => axioms(n,d.name,Type.OwlBool,r,f = true)
      case x                        => axioms(x.name,x.domain.name,Type.OwlString, x.req,f = true)
    }


  private def axioms
  (p: String, d: String, ra: OWL2Datatype, r: Boolean, f: Boolean) =
    axs(p,d,ra,r,f,isObjProp = false)


  private def axs(p: String, d: String, ra: OWL2Datatype,
                  r: Boolean, f: Boolean, isObjProp: Boolean)= {
    addPropertyToOntology(p, isObjProp)

    domainAxiom(p,d) | rangeAxiom(p,ra) | requiredAxiom(d,p,r,ra) | functionalAxiom(d,p,f)
  }


  private def axioms
  (p: String, d: String, ra: String, r: Boolean, f: Boolean, otherResSpec: String = m.name)=
    axs(p,d,ra,r,f,isObjProp = true,otherResSpec)


  private def axs(p: String, d: String, ra: String,
                  r: Boolean, f: Boolean, isObjProp: Boolean, otherResSpec: String = m.name)= {
    addPropertyToOntology(p, isObjProp)

    domainAxiom(p,d) | rangeAxiom(p,ra,otherResSpec) | requiredAxiom(d,p,r) | functionalAxiom(d,p,f)
  }



  private def owlPropertyFromGenericAttributeType(attributeType: GenericAttributeType) =
    attributeType match {
      case ListOfPrimitiveAttribute(pT,n,d) => attrAxioms(n,d.name,Type.fromString(pT),f = false)
      case IntAttribute(name,dom)     => attrAxioms(name,dom.name,Type.OwlInt,f = true)
      case FloatAttribute(name,dom)   => attrAxioms(name,dom.name,Type.OwlFloat,f = true)
      case DoubleAttribute(name,dom)  => attrAxioms(name,dom.name,Type.OwlDouble,f=true)
      case LongAttribute(name,dom)    => attrAxioms(name,dom.name,Type.OwlLong,f = true)
      case BooleanAttribute(name,dom) => attrAxioms(name,dom.name,Type.OwlBool,f = true)
      case x                          => attrAxioms(x.name,x.domain.name,Type.OwlString,f = true)
    }


  private def attrAxioms(a: String, d: String, ra: OWL2Datatype, f: Boolean): Set[OWLAxiom] = {
    addPropertyToOntology(a, isObjProp = false)

    domainAxiom(a,d) | rangeAxiom(a,ra) | functionalAxiom(d,a,f)
  }



  private def domainAxiom(p: String, d: String): Set[OWLAxiom] = {
    val domainClass = owlClass(d)
    owlOProp(p) match {
      case op if op != null && domainClass.isDefined
        => Set(m.df.getOWLObjectPropertyDomainAxiom(op, owlClass(d).get))
      case _ if domainClass.isDefined
        => Set(m.df.getOWLDataPropertyDomainAxiom(owlDProp(p), domainClass.get))
      case _
        => Set()
    }
  }




  private def rangeAxiom(p: String, r: String, otherResourceSpecName: String = m.name): Set[OWLAxiom] = {
    if (r==null || r=="null")
      Set()
    else if (owlClass(r,otherResourceSpecName).isDefined)
      Set(m.df.getOWLObjectPropertyRangeAxiom(owlOProp(p), owlClass(r, otherResourceSpecName).get))
    else
      Set()
  }



  private def rangeAxiom(p: String, r: OWL2Datatype): Set[OWLAxiom] =
    Set(m.df.getOWLDataPropertyRangeAxiom(owlDProp(p), r))



  private def requiredAxiom(d: String, p: String, r: Boolean, ra: OWL2Datatype = null): Set[OWLAxiom] =
    if (!r) Set()
    else
    {
      def rangeClassPresent = owlClass(d).isDefined
      owlOProp(p) match {
        case op if op != null && rangeClassPresent
        => Set(m.df
          .getOWLSubClassOfAxiom(
            owlClass(d).get,
            m.df.getOWLObjectSomeValuesFrom(op, m.df.getOWLThing)))
        case op if op == null && rangeClassPresent
        => Set(m.df
          .getOWLSubClassOfAxiom(
            owlClass(d).get,
            m.df.getOWLDataSomeValuesFrom(owlDProp(p), ra)))
      }
    }






  private def functionalAxiom(d: String, p: String, f: Boolean): Set[OWLAxiom] =
    if (!f) Set()
    else
      owlOProp(p) match {
      case op if op != null   => Set(m.df.getOWLFunctionalObjectPropertyAxiom(op))
      case _                  => Set(m.df.getOWLFunctionalDataPropertyAxiom(owlDProp(p)))
    }






  private def owlOProp(s: String) =
    if (m.ontology.containsObjectPropertyInSignature(ModelIRI.propertyTypeIRI(m.name, s)))
      m.df.getOWLObjectProperty(ModelIRI.propertyTypeIRI(m.name, s))
    else null





  private def owlDProp(s: String) =
    if (m.ontology.containsDataPropertyInSignature(ModelIRI.propertyTypeIRI(m.name, s)))
      m.df.getOWLDataProperty(ModelIRI.propertyTypeIRI(m.name, s))
    else null





  private def owlClass(resName: String, otherResSpec: String = m.name) = {
    if (resName==null)
      None
    else
      Some(m.df
        .getOWLClass(ModelIRI
          .resourceTypeIRI(
            otherResSpec, resName)))
  }






  private def addPropertyToOntology(pName: String, isObjProp: Boolean) =
    if (isObjProp) {
      val oProp = m.df.getOWLObjectProperty(ModelIRI.propertyTypeIRI(m.name, pName))

      m.ontology.add(m.df.getOWLDeclarationAxiom(oProp))

      m.ontology.addAxiom(
        m.df.getOWLAnnotationAssertionAxiom(
          oProp.getIRI,m.df.getRDFSLabel(resSpec.name+":"+pName)))
    }
    else {
      val dProp = m.df.getOWLDataProperty(ModelIRI.propertyTypeIRI(m.name, pName))
      m.ontology.add(m.df.getOWLDeclarationAxiom(dProp))

      m.ontology.addAxiom(
        m.df.getOWLAnnotationAssertionAxiom(
          dProp.getIRI,m.df.getRDFSLabel(resSpec.name+":"+pName)))
    }






  private def addResourceTypeToOntology(resName: String) = {
    val concept = m.df.getOWLClass(ModelIRI.resourceTypeIRI(m.name, resName))
    m.ontology.add(m.df.getOWLDeclarationAxiom(concept))

    m.ontology.addAxiom(
      m.df.getOWLAnnotationAssertionAxiom(
        concept.getIRI,m.df.getRDFSLabel(resSpec.name+":"+resName)))
  }







  private def addSubpropertyTypeToOntology(subpName: String)  = {
    val concept = m.df.getOWLClass(ModelIRI.subpropertyTypeIRI(m.name, subpName))
    m.ontology.add(m.df.getOWLDeclarationAxiom(concept))
    m.ontology.addAxiom(
      m.df.getOWLAnnotationAssertionAxiom(
        concept.getIRI,m.df.getRDFSLabel(resSpec.name+":"+subpName)))
  }



  private def mapAxioms
  (p: String, d: String, ra: String, r: Boolean, f: false, isDT : Boolean = false)= {

    def getMapEntry(ra : String) = {
      val e = m.df.getOWLClass(ModelIRI.mapEntryConceptIRI(m.name, ra))

      (e, Set(m.df.getOWLDeclarationAxiom(e)) )
    }

    def getMapEntryKey(e: OWLClass, ra:String)= {
      val k = m.df.getOWLDataProperty(ModelIRI.mapEntryKeyRoleIRI(m.name, ra.toString))

        Set(m.df.getOWLDataPropertyDomainAxiom(k,e)) ++
        Set(m.df.getOWLDataPropertyRangeAxiom(k,Type.fromString(ra))) ++
        Set(m.df.getOWLDeclarationAxiom(k))
    }

    def getMapEntryValue(e: OWLClass, ra:String, isDataProperty : Boolean = false)  =
      if (isDataProperty)
      {
        val v = m.df.getOWLDataProperty(ModelIRI.mapEntryValueRoleIRI(m.name, ra.toString))

          Set(m.df.getOWLDataPropertyDomainAxiom(v,e)) ++
          Set(m.df.getOWLDataPropertyRangeAxiom(v,Type.fromString(ra))) ++
          Set(m.df.getOWLDeclarationAxiom(v))

      }
      else
      {
        val v = m.df.getOWLObjectProperty( ModelIRI.mapEntryValueRoleIRI(m.name, ra) )
        val ranDef = owlClass(ra).isDefined

        Set(m.df.getOWLObjectPropertyDomainAxiom(v,e)) ++
        (
          if (ranDef) Set(m.df.getOWLObjectPropertyRangeAxiom(v,owlClass(ra).get))
        else Set()
        ) ++
        Set(m.df.getOWLDeclarationAxiom(v))
      }


    addPropertyToOntology(p,isObjProp = true)
    val pair = getMapEntry(ra)
    m.ontology.add(m.df.getOWLObjectPropertyRangeAxiom(owlOProp(p), pair._1))

      domainAxiom(p,d) ++
      getMapEntryKey(pair._1,ra) ++
      getMapEntryValue(pair._1,ra, isDataProperty = true) ++
      pair._2

  }


}