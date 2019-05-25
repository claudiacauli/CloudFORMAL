package aws.cfn.encoding.specification

import aws.cfn.dlmodel.specification.ResourceSpecificationModel
import aws.cfn.dlmodel.{Constants, Symbols}
import aws.cfn.formalization._
import org.semanticweb.owlapi.model.{OWLAxiom, OWLClass}
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
      case StringAttribute(name, dom) => attrAxioms(name, dom.name, Constants.OWL_STRING, f = true)
      case IntAttribute(name, dom) => attrAxioms(name, dom.name, Constants.OWL_INT, f = true)
      case FloatAttribute(name, dom) => attrAxioms(name, dom.name, Constants.OWL_FLOAT, f = true)
      case DoubleAttribute(name,dom) => attrAxioms(name,dom.name,Constants.OWL_DOUBLE, f=true)
      case LongAttribute(name, dom) => attrAxioms(name, dom.name, Constants.OWL_LONG, f = true)
      case CommaDelimitedListAttribute(name, dom) => attrAxioms(name, dom.name, Constants.OWL_STRING, f = true)
      case BooleanAttribute(name, dom) => attrAxioms(name, dom.name, Constants.OWL_BOOL, f = true)
      case DateTimeAttribute(name, dom) => attrAxioms(name, dom.name, Constants.OWL_STRING, f = true)
      case JsonAttribute(name, dom) => attrAxioms(name, dom.name, Constants.OWL_STRING, f = true)
      case ListOfPrimitiveAttribute(primitiveType, name, dom) => attrAxioms(name, dom.name, primitiveType, f = false)
    }
  }






  private def owlPropertyFromGenericPropertyType(propertyType: GenericPropertyType) =
    propertyType match {
      case StringProperty(name, dom, req) => axioms(name, dom.name, Constants.OWL_STRING, req, f = true)
      case IntProperty(name, dom, req) => axioms(name, dom.name, Constants.OWL_INT, req, f = true)
      case FloatProperty(name, dom, req) => axioms(name, dom.name, Constants.OWL_FLOAT, req, f = true)
      case DoubleProperty(name,dom,req) => axioms(name,dom.name,Constants.OWL_DOUBLE, req, f=true)
      case LongProperty(name, dom, req) => axioms(name, dom.name, Constants.OWL_LONG, req, f = true)
      case CommaDelimitedListProperty(name, dom, req) => axioms(name, dom.name, Constants.OWL_STRING, req, f = true)
      case BooleanProperty(name, dom, req) => axioms(name, dom.name, Constants.OWL_BOOL, req, f = true)
      case DateTimeProperty(name, dom, req) => axioms(name, dom.name, Constants.OWL_STRING, req, f = true)
      case JsonProperty(name, dom, req) => axioms(name, dom.name, Constants.OWL_STRING, req, f = true)
      case ListOfPrimitiveProperty(primitiveType, name, dom, req) => axioms(name, dom.name, Constants.fromString(primitiveType), req, f = false)
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
      case "string" => attrAxioms(a, d, Constants.OWL_STRING, f)
      case "float" => attrAxioms(a, d, Constants.OWL_FLOAT, f)
      case "double" => attrAxioms(a,d,Constants.OWL_DOUBLE,f)
      case "long" => attrAxioms(a, d, Constants.OWL_LONG, f)
      case "integer" => attrAxioms(a, d, Constants.OWL_INT, f)
      case "datetime" => attrAxioms(a, d, Constants.OWL_STRING, f)
      case "json" => attrAxioms(a, d, Constants.OWL_STRING, f)
      case "commadelimitedlist" => attrAxioms(a, d, Constants.OWL_STRING, f)
      case "boolean" => attrAxioms(a, d, Constants.OWL_BOOL, f)
      case _ => attrAxioms(a, d, Constants.OWL_STRING, f)
    }
  }







  private def axioms(p: String, d: String, ra: String, r: Boolean, f: Boolean, otherResourceSpecName: String = m.name): Set[OWLAxiom] =
    axs(p, d, ra, r, f, isObjProp = true, otherResourceSpecName)






  private def axioms(p: String, d: String, ra: OWL2Datatype, r: Boolean, f: Boolean): Set[OWLAxiom] =
    axs(p, d, ra, r, f, isObjProp = false)







  private def domainAxiom(p: String, d: String): Set[OWLAxiom] = {
    owlOProp(p) match {
      case op if op != null => Set(m.df.getOWLObjectPropertyDomainAxiom(op, owlClass(d)))
      case _ => Set(m.df.getOWLDataPropertyDomainAxiom(owlDProp(p), owlClass(d)))}
  }







  private def mapAxioms(p: String, d: String, ra: String, r: Boolean, f: false, isDT : Boolean = false) :Set[OWLAxiom]= {

    def getMapEntry(ra : String) = {
      val e = m.df.getOWLClass(Symbols.mapEntryConceptIRI(m.name, ra))
      (e, Set(m.df.getOWLDeclarationAxiom(e)) )
    }

    def getMapEntryKey(e: OWLClass, ra:String) = {
      val k = m.df.getOWLDataProperty(Symbols.mapEntryKeyRoleIRI(m.name, ra.toString))
      Set(m.df.getOWLDataPropertyDomainAxiom(k,e)) ++
        Set(m.df.getOWLDataPropertyRangeAxiom(k,Constants.fromString(ra))) ++
        Set(m.df.getOWLDeclarationAxiom(k))
    }

    def getMapEntryValue(e: OWLClass, ra:String, isDataProperty : Boolean = false)  =
    if (isDataProperty) {
      val v = m.df.getOWLDataProperty( Symbols.mapEntryValueRoleIRI(m.name, ra.toString) )
      Set(m.df.getOWLDataPropertyDomainAxiom(v,e)) ++
        Set(m.df.getOWLDataPropertyRangeAxiom(v,Constants.fromString(ra))) ++
        Set(m.df.getOWLDeclarationAxiom(v))
    } else {
      val v = m.df.getOWLObjectProperty( Symbols.mapEntryValueRoleIRI(m.name, ra) )
      Set(m.df.getOWLObjectPropertyDomainAxiom(v,e)) ++
        Set(m.df.getOWLObjectPropertyRangeAxiom(v,owlClass(ra))) ++
        Set(m.df.getOWLDeclarationAxiom(v))
    }

    addPropertyToOntology(p,isObjProp = true)
    val pair = getMapEntry(ra)
    m.ontology.add(m.df.getOWLObjectPropertyRangeAxiom(owlOProp(p), pair._1))
    domainAxiom(p,d) ++ getMapEntryKey(pair._1,ra) ++ getMapEntryValue(pair._1,ra, isDataProperty = true) ++ pair._2

  }







  private def rangeAxiom(p: String, r: String, otherResourceSpecName: String = m.name): Set[OWLAxiom] =
    if (r.equals("null"))
      Set(m.df.getOWLObjectPropertyRangeAxiom(owlOProp(p), m.df.getOWLThing))
    else
      Set(m.df.getOWLObjectPropertyRangeAxiom(owlOProp(p), owlClass(r, otherResourceSpecName)))






  private def rangeAxiom(p: String, r: OWL2Datatype): Set[OWLAxiom] =
    Set(m.df.getOWLDataPropertyRangeAxiom(owlDProp(p), r))






  private def requiredAxiom(d: String, p: String, r: Boolean, ra: OWL2Datatype = null): Set[OWLAxiom] =
    if (!r) Set()
    else owlOProp(p) match {
      case op if op != null => Set(m.df.getOWLSubClassOfAxiom(owlClass(d), m.df.getOWLObjectSomeValuesFrom(op, m.df.getOWLThing)))
      case _ => Set(m.df.getOWLSubClassOfAxiom(owlClass(d), m.df.getOWLDataSomeValuesFrom(owlDProp(p), ra)))
    }







  private def functionalAxiom(d: String, p: String, f: Boolean): Set[OWLAxiom] =
    if (!f) Set()
    else owlOProp(p) match {
      case op if op != null => Set(m.df.getOWLFunctionalObjectPropertyAxiom(op))
      case _ => Set(m.df.getOWLFunctionalDataPropertyAxiom(owlDProp(p)))
    }






  private def owlOProp(s: String) =
    if (m.ontology.containsObjectPropertyInSignature(Symbols.propertyTypeIRI(m.name, s)))
      m.df.getOWLObjectProperty(Symbols.propertyTypeIRI(m.name, s))
    else null





  private def owlDProp(s: String) =
    if (m.ontology.containsDataPropertyInSignature(Symbols.propertyTypeIRI(m.name, s)))
      m.df.getOWLDataProperty(Symbols.propertyTypeIRI(m.name, s))
    else null





  private def owlClass(resName: String, otherResSpec: String = m.name) = {
    if (resName.equals("policydocument"))
      m.df.getOWLClass(Symbols.resourceTypeIRI("policydocument", resName))
    else
      m.df.getOWLClass(Symbols.resourceTypeIRI(otherResSpec, resName))

  }






  private def addPropertyToOntology(pName: String, isObjProp: Boolean) =
    if (isObjProp)
      m.ontology.add(m.df.getOWLDeclarationAxiom(m.df.getOWLObjectProperty(Symbols.propertyTypeIRI(m.name, pName))))
    else
      m.ontology.add(m.df.getOWLDeclarationAxiom(m.df.getOWLDataProperty(Symbols.propertyTypeIRI(m.name, pName))))






  private def addResourceTypeToOntology(resName: String) =
    m.ontology.add(m.df.getOWLDeclarationAxiom(m.df.getOWLClass(Symbols.resourceTypeIRI(m.name, resName))))






  private def addSubpropertyTypeToOntology(subpName: String)  =
    m.ontology.add(m.df.getOWLDeclarationAxiom(m.df.getOWLClass(Symbols.subpropertyTypeIRI(m.name, subpName))))







}
