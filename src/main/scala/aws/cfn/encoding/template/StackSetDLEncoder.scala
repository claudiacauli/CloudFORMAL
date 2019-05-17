package aws.cfn.encoding.template

import aws.cfn.dlmodel.Symbols
import aws.cfn.dlmodel.template.StackSetModel
import aws.cfn.formalization.{AllowStatement, BooleanNode, CommaDelimitedListNode, DateTimeNode, DenyStatement, FloatNode, GenericValueNode, IntNode, JsonNode, LongNode, Node, ObjectNode, PolicyNode, ResourceNode, StackSet, StackSetNode, StringNode, SubpropertyNode, ValueNode}
import org.semanticweb.owlapi.model.{IRI, OWLAxiom, OWLAxiomVisitor, OWLClass, OWLClassExpression, OWLDataProperty, OWLIndividual, OWLNamedIndividual, OWLObjectProperty, OWLObjectPropertyAssertionAxiom}

import scala.jdk.OptionConverters._


object StackSetDLEncoder {

  def encode(stackSet: StackSet): StackSetModel = {
    new StackSetDLEncoder(stackSet).encode()
  }

}


class StackSetDLEncoder(stackSet: StackSet){

  val m : StackSetModel = new StackSetModel(stackSet.name)

  def encode(): StackSetModel ={

    /*
    TODO Populate the model m will the set of all axioms computed by instantiating all the things in the StackSet object!
     */

    m
  }

  /*
  TODO Write necessary encoding submethods, that correspond to encoding of all different parts of a template
   */

  private def encodeResource(resType: String, resource: ResourceNode): Unit = {


    def encodeProperty(sourceIndividual:OWLIndividual, propName: String, cfnNode: StackSetNode): Vector[OWLAxiom] = oProperty(propName) match {
      case Some(op) => encodeObjectProperty(sourceIndividual, op, cfnNode.asInstanceOf[ObjectNode])
      case None => dProperty(propName) match {
        case Some(dp) => encodeValueProperty(sourceIndividual,dp,cfnNode.asInstanceOf[ValueNode])
        case None => Vector()    // TODO ERROR! Something wrong
      }
    }

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
    }


    def encodeObjectProperty(sourceIndividual:OWLIndividual, objProp:OWLObjectProperty, objNode: ObjectNode) : Vector[OWLAxiom]
    = objNode match {
      case ResourceNode(resourceLogicalId, _, _) => Vector(m.df.getOWLObjectPropertyAssertionAxiom(objProp, sourceIndividual, individual(resourceLogicalId)))
      case SubpropertyNode(properties) => {
        val individualRandomIRI = Symbols.subpropertyBlankNodeIRI(stackSet.name)
        createBlankNode(objProp, individualRandomIRI) ++
          properties.toVector.flatMap(p => encodeProperty(
            m.df.getOWLNamedIndividual(individualRandomIRI), p._1, p._2))
      }
      case PolicyNode(statements) => {
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
      if (m.ontology.containsObjectPropertyInSignature(Symbols.propertyTypeIRI(resType, name)))
        Some(m.df.getOWLObjectProperty(Symbols.propertyTypeIRI(resType, name)))
      else None

    def dProperty(name:String): Option[OWLDataProperty] =
      if (m.ontology.containsDataPropertyInSignature(Symbols.propertyTypeIRI(resType, name)))
        Some(m.df.getOWLDataProperty(Symbols.propertyTypeIRI(resType, name)))
      else None

    def classFromIRI(iri:IRI) = m.df.getOWLClass(iri)
    def oPropFromIRI(iri:IRI) = m.df.getOWLObjectProperty(iri)





  }




}
