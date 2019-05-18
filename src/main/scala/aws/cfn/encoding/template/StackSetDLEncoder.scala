package aws.cfn.encoding.template

import aws.cfn.dlmodel.Symbols
import aws.cfn.dlmodel.template.StackSetModel
import aws.cfn.formalization._
import org.semanticweb.owlapi.model.{IRI, OWLAxiom, OWLClassExpression, OWLDataProperty, OWLIndividual, OWLObjectProperty}

import scala.jdk.OptionConverters._
import scala.jdk.CollectionConverters._


object StackSetDLEncoder {

  def encode(stackSet: StackSet): StackSetModel = {
    new StackSetDLEncoder(stackSet).encode()
  }

}


class StackSetDLEncoder(stackSet: StackSet){

  val m : StackSetModel = new StackSetModel(stackSet.name)

  def encode() : StackSetModel = {

    /*
    TODO Populate the model m will the set of all axioms computed by instantiating all the things in the StackSet object!
     */
    for ( t <- stackSet.templates; r <- t.resources.toVector ) yield {
      m.ontology.add(m.df.getOWLClassAssertionAxiom(
        classFromIRI(
          Symbols.resourceTypeIRI(r._2.serviceType, r._2.resourceType)),
          m.df.getOWLNamedIndividual(Symbols.resourceInstanceIRI(stackSet.name, r._1))
      ))
    }
    for ( t <- stackSet.templates; r <- t.resources.toVector ) yield {
      m.ontology.add(encodeResource(r._2).asJava)
    }

    m
  }

  /*
  TODO Write necessary encoding submethods, that correspond to encoding of all different parts of a template
   */

  private def encodeResource(resource: ResourceNode): Vector[OWLAxiom]  = {




//    def createResourceNode(iri: IRI) =
//      Vector ( m.df.getOWLClassAssertionAxiom(
//        classFromIRI(Symbols.resourceTypeIRI(resource.resourceType,resource.resourceLogicalId)), m.df.getOWLNamedIndividual(iri)) )


    def encodeAllGivenSubproperties(sourceIndividualIRI :IRI, givenProperties: Map[String,StackSetNode]):Vector[OWLAxiom] =
      givenProperties.toVector flatMap (p => encodeProperty(m.df.getOWLNamedIndividual(sourceIndividualIRI), p._1, p._2))



    def encodeAllAbsentSuproperties(sourceIndividualIRI: IRI, absentProperties: Vector[String]):Vector[OWLAxiom] =
      absentProperties flatMap (p => encodeAbsentProperty(m.df.getOWLNamedIndividual(sourceIndividualIRI), p))



    def encodeProperty(sourceIndividual:OWLIndividual, propName: String, cfnNode: StackSetNode): Vector[OWLAxiom] = oProperty(propName) match {
      case Some(op) => encodeObjectProperty(sourceIndividual, op, cfnNode.asInstanceOf[ObjectNode])
      case None => dProperty(propName) match {
        case Some(dp) => encodeValueProperty(sourceIndividual,dp,cfnNode.asInstanceOf[GenericValueNode])
        case None => Vector()    // TODO ERROR! Something wrong
      }
    }


    def encodeAbsentProperty(sourceIndividual:OWLIndividual, propName:String) : Vector[OWLAxiom] =
      Vector( m.df.getOWLSubClassOfAxiom( m.df.getOWLObjectOneOf(sourceIndividual),
        oProperty(propName) match {
          case Some(op) => m.df.getOWLObjectAllValuesFrom(op, m.df.getOWLNothing)
          case None =>  m.df.getOWLObjectComplementOf(m.df.getOWLDataSomeValuesFrom( dProperty(propName).get, m.df.getTopDatatype ))
        }
      ))

    /*
    Only used in case of policies
     */
    def encodePropertyWithIRI(sourceIndividual:OWLIndividual, propIRI: IRI, cfnNode: StackSetNode): Vector[OWLAxiom] =
      encodeObjectProperty(sourceIndividual, oPropFromIRI(propIRI), cfnNode.asInstanceOf[ObjectNode])




    def encodeValueProperty(sourceIndividual:OWLIndividual, dataProp:OWLDataProperty, valueNode: GenericValueNode) =
    valueNode match {
      case StringNode(v) =>     Vector( m.df.getOWLDataPropertyAssertionAxiom(dataProp,sourceIndividual,v) )
      case IntNode(v) =>        Vector( m.df.getOWLDataPropertyAssertionAxiom(dataProp,sourceIndividual,v) )
      case BooleanNode(v) =>    Vector( m.df.getOWLDataPropertyAssertionAxiom(dataProp,sourceIndividual,v) )
      case LongNode(v) =>       Vector( m.df.getOWLDataPropertyAssertionAxiom(dataProp,sourceIndividual,v) )
      case FloatNode(v) =>      Vector( m.df.getOWLDataPropertyAssertionAxiom(dataProp,sourceIndividual,v) )
      case DateTimeNode(v) =>   Vector( m.df.getOWLDataPropertyAssertionAxiom(dataProp,sourceIndividual,v) )
      case CommaDelimitedListNode(v) => Vector( m.df.getOWLDataPropertyAssertionAxiom(dataProp,sourceIndividual,v) )
      case JsonNode(v) =>       Vector( m.df.getOWLDataPropertyAssertionAxiom(dataProp,sourceIndividual,v) )
      case NoValue =>           Vector()
    }


    def encodeObjectProperty(sourceIndividual:OWLIndividual, objProp:OWLObjectProperty, objNode: ObjectNode) : Vector[OWLAxiom]
    = objNode match {
      case ResourceNode(resourceLogicalId, _,_,_, _,_) => Vector(m.df.getOWLObjectPropertyAssertionAxiom(objProp, sourceIndividual, individual(resourceLogicalId)))
      case SubpropertyNode(givenProperties,absentProperties) => {
        val individualRandomIRI = Symbols.subpropertyBlankNodeIRI(stackSet.name)
        createBlankNode(objProp, individualRandomIRI) ++
          encodeAllGivenSubproperties(individualRandomIRI,givenProperties) ++
          (absentProperties flatMap (p => encodeAbsentProperty(m.df.getOWLNamedIndividual(individualRandomIRI), p)))
      }
      case PolicyNode(statements) => {   // TODO Again!
        val policyRandomIRI = Symbols.policyNodeIRI(stackSet.name)
        createPolicyNode(policyRandomIRI) ++
          statements.flatMap( s => encodePropertyWithIRI( m.df.getOWLNamedIndividual(policyRandomIRI) ,
            Symbols.propertyTypeIRI("policydocument", "statement") , s) )
        }
      case AllowStatement(p,a,r,c) => null // TODO
      case DenyStatement(p,a,r,c) => null // TODO Continue from here and decide if you should move the instantion from here to another class!
      }



    def createBlankNode(objProp: OWLObjectProperty, individualRandomIRI:IRI): Vector[OWLAxiom] = range(objProp) match {
      case None =>    Vector(m.df.getOWLDeclarationAxiom(m.df.getOWLNamedIndividual(individualRandomIRI)))
      case Some(c) => Vector(m.df.getOWLClassAssertionAxiom(c, m.df.getOWLNamedIndividual(individualRandomIRI)))
    }

    def createPolicyNode(policyRandomIRI : IRI) : Vector[OWLAxiom] =
      Vector ( m.df.getOWLClassAssertionAxiom(
        classFromIRI(Symbols.policyDocIRI), m.df.getOWLNamedIndividual(Symbols.policyNodeIRI(stackSet.name))) )


    def individual(name:String) = m.df.getOWLNamedIndividual(Symbols.resourceInstanceIRI(m.name,name))

    def range(objProp: OWLObjectProperty) : Option[OWLClassExpression] = m.ontology.objectPropertyRangeAxioms(objProp).findFirst().toScala match {
      case Some(ax) => Some(ax.getRange)
      case None => None
    }

    def oProperty(name:String): Option[OWLObjectProperty] =
      if (m.ontology.containsObjectPropertyInSignature(Symbols.propertyTypeIRI(resource.resourceType, name)))
        Some(m.df.getOWLObjectProperty(Symbols.propertyTypeIRI(resource.resourceType, name)))
      else None

    def dProperty(name:String): Option[OWLDataProperty] =
      if (m.ontology.containsDataPropertyInSignature(Symbols.propertyTypeIRI(resource.resourceType, name)))
        Some(m.df.getOWLDataProperty(Symbols.propertyTypeIRI(resource.resourceType, name)))
      else None

    def oPropFromIRI(iri:IRI) = m.df.getOWLObjectProperty(iri)


    val resourceInstanceIRI = Symbols.resourceInstanceIRI(stackSet.name, resource.resourceLogicalId)
    //createResourceNode( resourceInstanceIRI ) ++
    encodeAllGivenSubproperties( resourceInstanceIRI, resource.givenProperties ) ++
      encodeAllAbsentSuproperties( resourceInstanceIRI, resource.absentProperties )

  }


  def classFromIRI(iri:IRI) = m.df.getOWLClass(iri)


}
