package aws.cfn.specifications.encoding

import aws.cfn.dlmodel.specification.ResourceSpecificationModel
import aws.cfn.dlmodel.{DLModelIRI, PrimitiveTypes}
import aws.cfn.specifications.formalization._
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.vocab.OWL2Datatype

import scala.jdk.CollectionConverters._


object Specification2DLEncoder {

  def encode(resourceSpecification: ResourceSpecification) : ResourceSpecificationModel =
    new Specification2DLEncoder(resourceSpecification).encode()

}


private class Specification2DLEncoder(resSpec : ResourceSpecification) {

  val m : ResourceSpecificationModel = new ResourceSpecificationModel(resSpec.name)

  def encode() : ResourceSpecificationModel = {
    m.ontology.add((owlClassFromResourceType(resSpec.resType) ++ (resSpec.subPropTypes flatMap owlClassFromSubpropertyType)).asJava)
    m
  }



  private def owlClassFromResourceType(resourceType: ResourceValueType): Vector[OWLAxiom] = {
    addResourceTypeToOntology(resourceType.name)
    (resourceType.attrs flatMap owlPropertyFromGenericAttributeType) ++
      (resourceType.props flatMap owlPropertyFromGenericPropertyType)
  }





  private def owlClassFromSubpropertyType(subpropertyType: SubPropertyValueType): Vector[OWLAxiom] = {
    addSubpropertyTypeToOntology(subpropertyType.name)
    subpropertyType.props flatMap owlPropertyFromGenericPropertyType
  }






  private def owlPropertyFromGenericAttributeType(attributeType: GenericAttributeType) = {
    attributeType match {
      case StringAttribute(name, dom) => attrAxioms(name, dom.name, PrimitiveTypes.OWL_STRING, f = true)
      case IntAttribute(name, dom) => attrAxioms(name, dom.name, PrimitiveTypes.OWL_INT, f = true)
      case FloatAttribute(name, dom) => attrAxioms(name, dom.name, PrimitiveTypes.OWL_FLOAT, f = true)
      case DoubleAttribute(name,dom) => attrAxioms(name,dom.name,PrimitiveTypes.OWL_DOUBLE, f=true)
      case LongAttribute(name, dom) => attrAxioms(name, dom.name, PrimitiveTypes.OWL_LONG, f = true)
      case CommaDelimitedListAttribute(name, dom) => attrAxioms(name, dom.name, PrimitiveTypes.OWL_STRING, f = true)
      case BooleanAttribute(name, dom) => attrAxioms(name, dom.name, PrimitiveTypes.OWL_BOOL, f = true)
      case TimeStamp(name, dom) => attrAxioms(name, dom.name, PrimitiveTypes.OWL_STRING, f = true)
      case JsonAttribute(name, dom) => attrAxioms(name, dom.name, PrimitiveTypes.OWL_STRING, f = true)
      case ListOfPrimitiveAttribute(primitiveType, name, dom) => attrAxioms(name, dom.name, primitiveType, f = false)
    }
  }






  private def owlPropertyFromGenericPropertyType(propertyType: GenericPropertyType) =
    propertyType match {
      case StringProperty(name, dom, req) => axioms(name, dom.name, PrimitiveTypes.OWL_STRING, req, f = true)
      case IntProperty(name, dom, req) => axioms(name, dom.name, PrimitiveTypes.OWL_INT, req, f = true)
      case FloatProperty(name, dom, req) => axioms(name, dom.name, PrimitiveTypes.OWL_FLOAT, req, f = true)
      case DoubleProperty(name,dom,req) => axioms(name,dom.name,PrimitiveTypes.OWL_DOUBLE, req, f=true)
      case LongProperty(name, dom, req) => axioms(name, dom.name, PrimitiveTypes.OWL_LONG, req, f = true)
      case CommaDelimitedListProperty(name, dom, req) => axioms(name, dom.name, PrimitiveTypes.OWL_STRING, req, f = true)
      case BooleanProperty(name, dom, req) => axioms(name, dom.name, PrimitiveTypes.OWL_BOOL, req, f = true)
      case TimeStampProperty(name, dom, req) => axioms(name, dom.name, PrimitiveTypes.OWL_STRING, req, f = true)
      case JsonProperty(name, dom, req) => axioms(name, dom.name, PrimitiveTypes.OWL_STRING, req, f = true)
      case ListOfPrimitiveProperty(primitiveType, name, dom, req) => axioms(name, dom.name, PrimitiveTypes.fromString(primitiveType), req, f = false)
      case ListOfNonPrimitiveProperty(nonPrimitiveType, name, dom, req) => axioms(name, dom.name, nonPrimitiveType, req, f = false)
      case MapOfPrimitiveProperty(primitiveType, name, dom, req) => mapAxioms(name, dom.name, primitiveType, req, f = false, isDT = true)
      case MapOfNonPrimitiveProperty(nonPrimitiveType, name, dom, req) => mapAxioms(name, dom.name, nonPrimitiveType, req, f = false)
      case SubpropertyProperty(rangeName, propName, dom, req) => axioms(propName, dom.name, rangeName, req, f = true)
      case ResourceProperty(otherResSpecName, resTypeName, name, dom, req) => axioms(name, dom.name, resTypeName, req, f = true, otherResSpecName)
      case ListOfResourcesProperty(otherResSpecName,resTypeName,name,dom,req) => axioms(name,dom.name,resTypeName,req, f = false, otherResSpecName)
      case PolicyProperty(name, dom, req) => axioms(name, dom.name, "policydocument", req, f = true)
    }






  private def axs(p: String, d: String, ra: String,
                  r: Boolean, f: Boolean, isObjProp: Boolean, otherResourceSpecName: String = m.name)= {
    addPropertyToOntology(p, isObjProp)
    domainAxiom(p, d) ++ rangeAxiom(p, ra, otherResourceSpecName) ++ requiredAxiom(d, p, r) ++ functionalAxiom(d, p, f)
  }






  private def axs(p: String, d: String, ra: OWL2Datatype,
                  r: Boolean, f: Boolean, isObjProp: Boolean)= {
    addPropertyToOntology(p, isObjProp)
    domainAxiom(p, d) ++ rangeAxiom(p, ra) ++ requiredAxiom(d, p, r, ra) ++ functionalAxiom(d, p, f)
  }






  private def attrAxioms(a: String, d: String, ra: OWL2Datatype, f: Boolean): Set[OWLAxiom] = {
    addPropertyToOntology(a, isObjProp = false)
    domainAxiom(a, d) ++ rangeAxiom(a, ra) ++ functionalAxiom(d, a, f)
  }







  private def attrAxioms(a: String, d: String, ra: String, f: Boolean): Set[OWLAxiom] = {
    ra match {
      case "string" => attrAxioms(a, d, PrimitiveTypes.OWL_STRING, f)
      case "float" => attrAxioms(a, d, PrimitiveTypes.OWL_FLOAT, f)
      case "double" => attrAxioms(a,d,PrimitiveTypes.OWL_DOUBLE,f)
      case "long" => attrAxioms(a, d, PrimitiveTypes.OWL_LONG, f)
      case "integer" => attrAxioms(a, d, PrimitiveTypes.OWL_INT, f)
      case "timestamp" => attrAxioms(a, d, PrimitiveTypes.OWL_STRING, f)
      case "json" => attrAxioms(a, d, PrimitiveTypes.OWL_STRING, f)
      case "commadelimitedlist" => attrAxioms(a, d, PrimitiveTypes.OWL_STRING, f)
      case "boolean" => attrAxioms(a, d, PrimitiveTypes.OWL_BOOL, f)
      case _ => attrAxioms(a, d, PrimitiveTypes.OWL_STRING, f)
    }
  }







  private def axioms(p: String, d: String, ra: String, r: Boolean, f: Boolean, otherResourceSpecName: String = m.name): Set[OWLAxiom] =
    axs(p, d, ra, r, f, isObjProp = true, otherResourceSpecName)






  private def axioms(p: String, d: String, ra: OWL2Datatype, r: Boolean, f: Boolean): Set[OWLAxiom] =
    axs(p, d, ra, r, f, isObjProp = false)







  private def domainAxiom(p: String, d: String): Set[OWLAxiom] = {
    val domainClass = owlClass(d)
    owlOProp(p) match {
      case op if op != null && domainClass.isDefined => Set(m.df.getOWLObjectPropertyDomainAxiom(op, owlClass(d).get))
      case _ if domainClass.isDefined => Set(m.df.getOWLDataPropertyDomainAxiom(owlDProp(p), domainClass.get))
      case _ => Set()
    }
  }







  private def mapAxioms(p: String, d: String, ra: String, r: Boolean, f: false, isDT : Boolean = false) :Set[OWLAxiom]= {

    def getMapEntry(ra : String) = {
      val e = m.df.getOWLClass(DLModelIRI.mapEntryConceptIRI(m.name, ra))
      (e, Set(m.df.getOWLDeclarationAxiom(e)) )
    }

    def getMapEntryKey(e: OWLClass, ra:String) = {
      val k = m.df.getOWLDataProperty(DLModelIRI.mapEntryKeyRoleIRI(m.name, ra.toString))
      Set(m.df.getOWLDataPropertyDomainAxiom(k,e)) ++
        Set(m.df.getOWLDataPropertyRangeAxiom(k,PrimitiveTypes.fromString(ra))) ++
        Set(m.df.getOWLDeclarationAxiom(k))
    }

    def getMapEntryValue(e: OWLClass, ra:String, isDataProperty : Boolean = false)  =
    if (isDataProperty) {
      val v = m.df.getOWLDataProperty( DLModelIRI.mapEntryValueRoleIRI(m.name, ra.toString) )
      Set(m.df.getOWLDataPropertyDomainAxiom(v,e)) ++
        Set(m.df.getOWLDataPropertyRangeAxiom(v,PrimitiveTypes.fromString(ra))) ++
        Set(m.df.getOWLDeclarationAxiom(v))
    } else {
      val v = m.df.getOWLObjectProperty( DLModelIRI.mapEntryValueRoleIRI(m.name, ra) )
      val ranDef = owlClass(ra).isDefined
      Set(m.df.getOWLObjectPropertyDomainAxiom(v,e)) ++
        (if (ranDef) Set(m.df.getOWLObjectPropertyRangeAxiom(v,owlClass(ra).get))
      else Set()) ++
        Set(m.df.getOWLDeclarationAxiom(v))
    }

    addPropertyToOntology(p,isObjProp = true)
    val pair = getMapEntry(ra)
    m.ontology.add(m.df.getOWLObjectPropertyRangeAxiom(owlOProp(p), pair._1))
    domainAxiom(p,d) ++ getMapEntryKey(pair._1,ra) ++ getMapEntryValue(pair._1,ra, isDataProperty = true) ++ pair._2

  }







  private def rangeAxiom(p: String, r: String, otherResourceSpecName: String = m.name): Set[OWLAxiom] = {

    if (r==null || r=="null")
      Set()
    else if (owlClass(r,otherResourceSpecName).isDefined)
      Set(m.df.getOWLObjectPropertyRangeAxiom(owlOProp(p), owlClass(r, otherResourceSpecName).get))
    else Set()

  }







  private def rangeAxiom(p: String, r: OWL2Datatype): Set[OWLAxiom] =
    Set(m.df.getOWLDataPropertyRangeAxiom(owlDProp(p), r))






  private def requiredAxiom(d: String, p: String, r: Boolean, ra: OWL2Datatype = null): Set[OWLAxiom] =
    if (!r) Set()
    else {
      def rangeClassPresent = owlClass(d).isDefined
      owlOProp(p) match {
        case op if op != null && rangeClassPresent => Set(m.df.getOWLSubClassOfAxiom(owlClass(d).get, m.df.getOWLObjectSomeValuesFrom(op, m.df.getOWLThing)))
        case op if op == null & rangeClassPresent => Set(m.df.getOWLSubClassOfAxiom(owlClass(d).get, m.df.getOWLDataSomeValuesFrom(owlDProp(p), ra)))
      }

    }






  private def functionalAxiom(d: String, p: String, f: Boolean): Set[OWLAxiom] =
    if (!f) Set()
    else owlOProp(p) match {
      case op if op != null => Set(m.df.getOWLFunctionalObjectPropertyAxiom(op))
      case _ => Set(m.df.getOWLFunctionalDataPropertyAxiom(owlDProp(p)))
    }






  private def owlOProp(s: String) =
    if (m.ontology.containsObjectPropertyInSignature(DLModelIRI.propertyTypeIRI(m.name, s)))
      m.df.getOWLObjectProperty(DLModelIRI.propertyTypeIRI(m.name, s))
    else null





  private def owlDProp(s: String) =
    if (m.ontology.containsDataPropertyInSignature(DLModelIRI.propertyTypeIRI(m.name, s)))
      m.df.getOWLDataProperty(DLModelIRI.propertyTypeIRI(m.name, s))
    else null





  private def owlClass(resName: String, otherResSpec: String = m.name) = {
    if (resName==null)
      None
    else if (resName.equals("policydocument"))
      Some(m.df.getOWLClass(DLModelIRI.resourceTypeIRI("policydocument", resName)))
    else
      Some(m.df.getOWLClass(DLModelIRI.resourceTypeIRI(otherResSpec, resName)))

  }






  private def addPropertyToOntology(pName: String, isObjProp: Boolean) =
    if (isObjProp) {
      val oProp = m.df.getOWLObjectProperty(DLModelIRI.propertyTypeIRI(m.name, pName))
      addLabel(oProp,pName)
      m.ontology.add(m.df.getOWLDeclarationAxiom(oProp))
    }
    else {
      val dProp = m.df.getOWLDataProperty(DLModelIRI.propertyTypeIRI(m.name, pName))
      addLabel(dProp,pName)
      m.ontology.add(m.df.getOWLDeclarationAxiom(dProp))
    }






  private def addResourceTypeToOntology(resName: String) = {
    val concept = m.df.getOWLClass(DLModelIRI.resourceTypeIRI(m.name, resName))
    addLabel(concept, resName)
    m.ontology.add(m.df.getOWLDeclarationAxiom(concept))
  }







  private def addSubpropertyTypeToOntology(subpName: String)  = {
    val concept = m.df.getOWLClass(DLModelIRI.subpropertyTypeIRI(m.name, subpName))
    addLabel(concept, subpName)
    m.ontology.add(m.df.getOWLDeclarationAxiom(concept))
  }






  private def addLabel(concept: OWLClass, label:String): Unit =
    addLabel(concept.getIRI,label)

  private def addLabel(oProp : OWLObjectProperty, label:String) : Unit =
    addLabel(oProp.getIRI,label.split("_").last)

  private def addLabel(dProp: OWLDataProperty, label:String): Unit =
    addLabel(dProp.getIRI,label.split("_").last)

  private def addLabel(iri: IRI, label:String) : Unit = {
    val labelAxiom  = m.df.getOWLAnnotationAssertionAxiom( iri , m.df.getRDFSLabel(label) )
    m.manager.applyChange( new AddAxiom( m.ontology, labelAxiom ))
  }

}
