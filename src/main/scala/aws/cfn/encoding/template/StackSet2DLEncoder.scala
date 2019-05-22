package aws.cfn.encoding.template

import aws.cfn.dlmodel.Symbols
import aws.cfn.dlmodel.template.StackSetModel
import aws.cfn.formalization._
import org.semanticweb.owlapi.model.{IRI, OWLAxiom, OWLClass, OWLClassExpression, OWLDataProperty, OWLIndividual, OWLObjectProperty, OWLOntology, OWLOntologyManager}

import scala.jdk.OptionConverters._
import scala.jdk.CollectionConverters._


object StackSet2DLEncoder {

  def encode(stackSet: StackSet): StackSetModel = {
    new StackSet2DLEncoder(stackSet).encode()
  }

}


class StackSet2DLEncoder(stackSet: StackSet){

  val m : StackSetModel = new StackSetModel(stackSet.name, stackSet.manager.ontologies().toArray().toVector flatMap ( o => Set(o.asInstanceOf[OWLOntology]) ))

  def encode() : StackSetModel = {

    for ( t <- stackSet.templates; r <- t.resources.toVector ) yield {
      m.ontology.add(m.df.getOWLClassAssertionAxiom(
        classFromIRI(
          Symbols.resourceTypeIRI(r._2.serviceType+r._2.resourceType, r._2.resourceType)),
          m.df.getOWLNamedIndividual(Symbols.resourceInstanceIRI(stackSet.name, r._1))
      ))
    }
    for ( t <- stackSet.templates; r <- t.resources.toVector ) yield {
      m.ontology.add(encodeResource(r._2).asJava)
    }

    m
  }


  private def encodeResource(resource: ResourceNode): Vector[OWLAxiom]  = {


    //    def createResourceNode(iri: IRI) =
    //      Vector ( m.df.getOWLClassAssertionAxiom(
    //        classFromIRI(Symbols.resourceTypeIRI(resource.resourceType,resource.resourceLogicalId)), m.df.getOWLNamedIndividual(iri)) )


    def encodeAllGivenSubproperties(sourceIndividualIRI: IRI, givenProperties: Map[String, Node]): Vector[OWLAxiom] = {
      givenProperties.toVector flatMap (p => encodeProperty(m.df.getOWLNamedIndividual(sourceIndividualIRI), p._1, p._2))
    }


    def encodeAllAbsentSuproperties(sourceIndividualIRI: IRI, absentProperties: Set[String]): Vector[OWLAxiom] =
      absentProperties.toVector flatMap (p => encodeAbsentProperty(m.df.getOWLNamedIndividual(sourceIndividualIRI), p))


    def encodeProperty(sourceIndividual: OWLIndividual, propName: String, cfnNode: Node): Vector[OWLAxiom] = oProperty(propName) match {
      case Some(op) => {
        cfnNode match {
         case ListNode(vec) => vec flatMap (n => {
           n match {
             case StringNode(s) => encodeObjectProperty(sourceIndividual,op,ForeignNode(s))
             case NoValue => Vector()
             case _ => encodeObjectProperty(sourceIndividual,op,n.asInstanceOf[ObjectNode])
           }
         })
         case MapNode(m) => m.toVector flatMap (e => encodeObjectMapEntry(sourceIndividual, op, e))
         case NoValue => Vector()
         case _ => encodeObjectProperty(sourceIndividual, op,
           if (cfnNode.isInstanceOf[StringNode])
             ForeignNode(cfnNode.asInstanceOf[StringNode].value)
           else cfnNode.asInstanceOf[ObjectNode])
       }
      }
      case None => dProperty(propName) match {
        case Some(dp) => { cfnNode match {
          case ListNode(vec) => {
            vec flatMap (n => encodeValueProperty(sourceIndividual,dp,n.asInstanceOf[GenericValueNode]))
          }
          case MapNode(m) => m.toVector flatMap (e => encodeValueMapEntry(sourceIndividual,dp,e.asInstanceOf[(String,GenericValueNode)]))
          case NoValue => Vector()
          case ResourceNode(id,s,r,_) => {

            Vector()
          }
          case _ => encodeValueProperty(sourceIndividual, dp, cfnNode.asInstanceOf[GenericValueNode])
          }
        }
        case None => Vector() // TODO ERROR! Something wrong
      }
    }


    def encodeValueMapEntry(sourceIndividual:OWLIndividual, dp:OWLDataProperty, entry:(String,GenericValueNode) )
      : Vector[OWLAxiom] = {
      Vector() // TODO
    }

    def encodeObjectMapEntry(sourceIndividual: OWLIndividual, op:OWLObjectProperty, entry:(String,Node))
      :Vector[OWLAxiom] = {
      Vector() // TODO
    }


    def encodeAbsentProperty(sourceIndividual: OWLIndividual, propName: String): Vector[OWLAxiom] = {
      Vector(m.df.getOWLSubClassOfAxiom(m.df.getOWLObjectOneOf(sourceIndividual),
        oProperty(propName) match {
          case None => m.df.getOWLObjectComplementOf(m.df.getOWLDataSomeValuesFrom(dProperty(propName).get, m.df.getTopDatatype))
          case Some(op) => m.df.getOWLObjectAllValuesFrom(op, m.df.getOWLNothing)
        }
      ))
    }


    /*
    Only used in case of policies
     */
    def encodePropertyWithIRI(sourceIndividual: OWLIndividual, propIRI: IRI, cfnNode: Node): Vector[OWLAxiom] =
      encodeObjectProperty(sourceIndividual, oPropFromIRI(propIRI), cfnNode.asInstanceOf[ObjectNode])


    def encodeValueProperty(sourceIndividual: OWLIndividual, dataProp: OWLDataProperty, valueNode: GenericValueNode) =
      valueNode match {
        case StringNode(v) => Vector(m.df.getOWLDataPropertyAssertionAxiom(dataProp, sourceIndividual, v))
        case IntNode(v) => Vector(m.df.getOWLDataPropertyAssertionAxiom(dataProp, sourceIndividual, v))
        case BooleanNode(v) => Vector(m.df.getOWLDataPropertyAssertionAxiom(dataProp, sourceIndividual, v))
        case LongNode(v) => Vector(m.df.getOWLDataPropertyAssertionAxiom(dataProp, sourceIndividual, v))
        case FloatNode(v) => Vector(m.df.getOWLDataPropertyAssertionAxiom(dataProp, sourceIndividual, v))
        case DateTimeNode(v) => Vector(m.df.getOWLDataPropertyAssertionAxiom(dataProp, sourceIndividual, v))
        case CommaDelimitedListNode(v) => Vector(m.df.getOWLDataPropertyAssertionAxiom(dataProp, sourceIndividual, v))
        case JsonNode(v) => Vector(m.df.getOWLDataPropertyAssertionAxiom(dataProp, sourceIndividual, v))
        case NoValue => Vector()
        case ListNode(vec) => Vector() // TODO Generate axioms for list properties
        case MapNode(map) => Vector() // TODO Generate axioms for map properties
      }


    def encodeObjectProperty(sourceIndividual: OWLIndividual, objProp: OWLObjectProperty, objNode: ObjectNode): Vector[OWLAxiom]
    = objNode match {
      case ResourceNode(resourceLogicalId, _, _, _) => Vector(m.df.getOWLObjectPropertyAssertionAxiom(objProp, sourceIndividual, individual(resourceLogicalId)))
      case SubpropertyNode(givenProperties, absentProperties) => {
        val individualRandomIRI = Symbols.subpropertyBlankNodeIRI(stackSet.name)
        createBlankNode(sourceIndividual,objProp, individualRandomIRI) ++
          encodeAllGivenSubproperties(individualRandomIRI, givenProperties) ++
          (absentProperties flatMap (p => encodeAbsentProperty(m.df.getOWLNamedIndividual(individualRandomIRI), p)))
      }
      case PolicyNode(statements) => { // TODO Again!
        val policyRandomIRI = Symbols.policyNodeIRI(stackSet.name)
        createPolicyNode(policyRandomIRI) ++
          statements.flatMap(s => encodePropertyWithIRI(m.df.getOWLNamedIndividual(policyRandomIRI),
            Symbols.propertyTypeIRI("policydocument", "statement"), s))
      }
      case ForeignNode(v) => Vector(m.df.getOWLObjectPropertyAssertionAxiom(objProp, sourceIndividual, individual(v)))
      case AllowStatement(p, a, r, c) => null // TODO
      case DenyStatement(p, a, r, c) => null // TODO Continue from here and decide if you should move the instantion from here to another class!
    }


    def createBlankNode(sourceIndividual: OWLIndividual, objProp: OWLObjectProperty, individualRandomIRI: IRI): Vector[OWLAxiom] = {
        range(objProp) match {
          case None => {
            val newNode = m.df.getOWLNamedIndividual(individualRandomIRI)
            Vector(m.df.getOWLObjectPropertyAssertionAxiom(objProp,sourceIndividual,newNode)) ++
            Vector(m.df.getOWLDeclarationAxiom(newNode))
          }
          case Some(c) => {
            val newNode = m.df.getOWLNamedIndividual(individualRandomIRI)
            Vector(m.df.getOWLObjectPropertyAssertionAxiom(objProp,sourceIndividual,newNode)) ++
            Vector(m.df.getOWLClassAssertionAxiom(c,newNode))
          }
        }
      }


    def createPolicyNode(policyRandomIRI : IRI) : Vector[OWLAxiom] =
      Vector ( m.df.getOWLClassAssertionAxiom(
        classFromIRI(Symbols.policyDocIRI), m.df.getOWLNamedIndividual(Symbols.policyNodeIRI(stackSet.name))) )


    def individual(name:String) = m.df.getOWLNamedIndividual(Symbols.resourceInstanceIRI(m.name,name))

    def range(objProp: OWLObjectProperty) : Option[OWLClass] = {
      m.manager.getOntology(Symbols.resourceTerminologyIRI(resource.serviceType+resource.resourceType)).objectPropertyRangeAxioms(objProp).findFirst().toScala match {
        case Some(ax) => Some(ax.getRange.asOWLClass())
        case None => None
      }
    }

    def oProperty(name:String): Option[OWLObjectProperty] = {
      if (m.manager.getOntology(Symbols.resourceTerminologyIRI(resource.serviceType+resource.resourceType)).containsObjectPropertyInSignature(Symbols.propertyTypeIRI(resource.serviceType + resource.resourceType, name)))
        Some(m.df.getOWLObjectProperty(Symbols.propertyTypeIRI(resource.serviceType+resource.resourceType, name)))
      else None
    }

    def dProperty(name:String): Option[OWLDataProperty] = {
      if (m.manager.getOntology(Symbols.resourceTerminologyIRI(resource.serviceType+resource.resourceType )).containsDataPropertyInSignature(Symbols.propertyTypeIRI(resource.serviceType+resource.resourceType, name)))
        Some(m.df.getOWLDataProperty(Symbols.propertyTypeIRI(resource.serviceType+resource.resourceType, name)))
      else None
    }


    def oPropFromIRI(iri:IRI) = m.df.getOWLObjectProperty(iri)


    val resourceInstanceIRI = Symbols.resourceInstanceIRI(stackSet.name, resource.resourceLogicalId)
    //createResourceNode( resourceInstanceIRI ) ++
    encodeAllGivenSubproperties( resourceInstanceIRI, resource.givenProperties ) ++
      encodeAllAbsentSuproperties( resourceInstanceIRI, resource.absentProperties )

  }


  def classFromIRI(iri:IRI) = m.df.getOWLClass(iri)


}
