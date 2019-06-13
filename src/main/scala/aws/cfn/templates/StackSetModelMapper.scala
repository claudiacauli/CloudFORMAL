package aws.cfn.templates

import aws.cfn.dlmodel.{AwsOntology, ModelIRI}
import org.semanticweb.owlapi.model._

import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._


protected object StackSetModelMapper {

  def encode(stackSet: StackSet, infrastructure: Infrastructure): StackSetModel = {
    new StackSetModelMapper(stackSet, infrastructure).encode()
  }

}


private class StackSetModelMapper(stackSet: StackSet, infrastructure: Infrastructure){

  val m : StackSetModel = new StackSetModel(stackSet.name, stackSet.manager.ontologies().toArray().toVector flatMap ( o => Set(o.asInstanceOf[OWLOntology]) ))

  def encode() : StackSetModel = {

    stackSet.templates foreach ( t => {

      val stackIndividualName = stackSet.name + t.name + AwsOntology.StackSuffix

      m.ontology.add(
        m.df.getOWLClassAssertionAxiom(
          classFromIRI(ModelIRI.awsConceptIRI(AwsOntology.StackSuffix)),
          m.df.getOWLNamedIndividual(ModelIRI.awsIndividualIRI( stackIndividualName ))
        ))

      m.ontology.add(
        m.df.getOWLObjectPropertyAssertionAxiom(
          m.df.getOWLObjectProperty(ModelIRI.awsPropertyIRI(AwsOntology.IsDeployedIn)),
          m.df.getOWLNamedIndividual(ModelIRI.awsIndividualIRI(stackIndividualName)),
          m.df.getOWLNamedIndividual(ModelIRI.awsAccountIRI(
            t.parameters(PseudoParameter.AccountId).asInstanceOf[StringNode].value))
        )
      )

    })

    for ( t <- stackSet.templates; r <- t.resources.toVector ) yield {

      m.ontology.add(m.df.getOWLClassAssertionAxiom(
        classFromIRI(
          ModelIRI.resourceTypeIRI(r._2.serviceType+r._2.resourceType, r._2.resourceType)),
            getResourceIndividual(r._1)
      ))

      m.ontology.add(m.df.getOWLObjectPropertyAssertionAxiom(
        m.df.getOWLObjectProperty(ModelIRI.awsPropertyIRI(AwsOntology.IsInStack)),
        getResourceIndividual(r._1),
        m.df.getOWLNamedIndividual(ModelIRI.awsIndividualIRI(stackSet.name + t.name + AwsOntology.StackSuffix))
      ))

    }

    infrastructure.externalResources foreach ( eR =>
      m.ontology.add(
        m.df.getOWLClassAssertionAxiom(
          classFromIRI(ModelIRI.awsConceptIRI(AwsOntology.ExternalResource)),
          m.df.getOWLNamedIndividual(ModelIRI.externalEntityIRI(infrastructure.name,eR.name))
        )
      ))


    for ( t <- stackSet.templates; r <- t.resources.toVector ) yield {
      m.ontology.add(encodeResource(r._2).asJava)
    }

    m
  }


  private def getResourceIndividual(resId: String) : OWLNamedIndividual = {
    val newResource = m.df.getOWLNamedIndividual(ModelIRI.resourceInstanceIRI(stackSet.name, resId))
    addComment(newResource, AwsOntology.ResourceAnnotationComment)
    newResource
  }


  private def encodeResource(resource: StackSetResource): Vector[OWLAxiom]  = {


    //    def createResourceNode(iri: IRI) =
    //      Vector ( m.df.getOWLClassAssertionAxiom(
    //        classFromIRI(Symbols.resourceTypeIRI(resource.resourceType,resource.resourceLogicalId)), m.df.getOWLNamedIndividual(iri)) )


    def encodeAllGivenSubproperties(sourceIndividualIRI: IRI, givenProperties: Map[String, Node]): Vector[OWLAxiom] = {
      givenProperties.toVector flatMap (p => encodeProperty(m.df.getOWLNamedIndividual(sourceIndividualIRI), p._1, p._2))
    }


    def encodeAllAbsentSuproperties(sourceIndividualIRI: IRI, absentProperties: Set[String]): Vector[OWLAxiom] =
      absentProperties.toVector flatMap (p => encodeAbsentProperty(m.df.getOWLNamedIndividual(sourceIndividualIRI), p))


    def encodeProperty(sourceIndividual: OWLIndividual, propName: String, cfnNode: Node): Vector[OWLAxiom] =

      oProperty(propName) match {

        case Some(op) =>
          cfnNode match {
           case ListNode(vec) => vec flatMap {
             case StringNode(s) => encodeObjectProperty(sourceIndividual, op, ExternalResource(s,infrastructure))
             case NoValue => Vector()
             case n => encodeObjectProperty(sourceIndividual, op, n.asInstanceOf[ObjectNode])
            }
           case MapNode(map) => map.toVector flatMap (e => encodeObjectMapEntry(sourceIndividual, op, e))
           case NoValue => Vector()
           case _ => encodeObjectProperty(sourceIndividual, op,
             cfnNode match {
               case StringNode(s) => ExternalResource(s,infrastructure)
               case _ => cfnNode.asInstanceOf[ObjectNode]
             })
         }

        case None => dProperty(propName) match {
          case Some(dp) => cfnNode match {
            case ListNode(vec) =>
              vec flatMap (n => encodeValueProperty(sourceIndividual,dp,n.asInstanceOf[GenericValueNode]))
            case MapNode(map) => map.toVector flatMap (e => encodeValueMapEntry(sourceIndividual,dp,e.asInstanceOf[(String,GenericValueNode)]))
            case NoValue => Vector()
            case StackSetResource(_,s,r,_,_) =>
              println("Data Property " + dp.toString.split(AwsOntology.Pound).last + " of " + resource.serviceType + resource.resourceType + AwsOntology.Pound + resource.resourceType + " points to resource of type "
                + s + r + AwsOntology.Pound + r )
              Vector() // TODO ERROR! Something wrong here: Property seems data but points to a resource
            case _ => encodeValueProperty(sourceIndividual, dp, cfnNode.asInstanceOf[GenericValueNode])
            }
          case None => Vector() // TODO ERROR! Something wrong here: Property neither obj or data!
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
//    def encodePropertyWithIRI(sourceIndividual: OWLIndividual, propIRI: IRI, cfnNode: Node): Vector[OWLAxiom] =
//      encodeObjectProperty(sourceIndividual, oPropFromIRI(propIRI), cfnNode.asInstanceOf[ObjectNode])


    def encodeValueProperty(sourceIndividual: OWLIndividual, dataProp: OWLDataProperty, valueNode: GenericValueNode) : Vector[OWLAxiom] = {

      valueNode match {
        case StringNode(v) => Vector(m.df.getOWLDataPropertyAssertionAxiom(dataProp, sourceIndividual, v))
        case IntNode(v) => Vector(m.df.getOWLDataPropertyAssertionAxiom(dataProp, sourceIndividual, v))
        case BooleanNode(v) => Vector(m.df.getOWLDataPropertyAssertionAxiom(dataProp, sourceIndividual, v))
        case LongNode(v) => Vector(m.df.getOWLDataPropertyAssertionAxiom(dataProp, sourceIndividual, v))
        case FloatNode(v) => Vector(m.df.getOWLDataPropertyAssertionAxiom(dataProp, sourceIndividual, v))
        case DoubleNode(v) => Vector(m.df.getOWLDataPropertyAssertionAxiom(dataProp,sourceIndividual,v))
        case TimeStampNode(v) => Vector(m.df.getOWLDataPropertyAssertionAxiom(dataProp, sourceIndividual, v))
        case CommaDelimitedListNode(v) => Vector(m.df.getOWLDataPropertyAssertionAxiom(dataProp, sourceIndividual, v))
        case JsonNode(v) => Vector(m.df.getOWLDataPropertyAssertionAxiom(dataProp, sourceIndividual, v))
        case NoValue => Vector()
        case ListNode(vec: Vector[Node]) =>
          vec flatMap (n => encodeValueProperty(sourceIndividual,dataProp,n.asInstanceOf[GenericValueNode]))//Vector() // TODO Generate axioms for list properties
        case MapNode(map) =>
          map.toVector flatMap (entry => encodeValueMapEntry(sourceIndividual,dataProp,entry.asInstanceOf[(String,GenericValueNode)]))// TODO Generate axioms for map properties
        case _ =>
          println("Evaluation returned: " + valueNode + " which is not a value node. Therefore property " + dataProp + " must not be a real data property.")
          Vector()
      }
    }



    def encodeObjectProperty(sourceIndividual: OWLIndividual, objProp: OWLObjectProperty, objNode: ObjectNode): Vector[OWLAxiom]
    = objNode match {
      case StackSetResource(resourceLogicalId,_,_,ss,_) => Vector(m.df.getOWLObjectPropertyAssertionAxiom(objProp, sourceIndividual, individualFromStackSet(resourceLogicalId,ss)))
      case Subproperty(givenProperties, absentProperties) =>
        val individualRandomIRI = ModelIRI.subpropertyBlankNodeIRI(stackSet.name)
        createBlankNode(sourceIndividual,objProp, individualRandomIRI) ++
          encodeAllGivenSubproperties(individualRandomIRI, givenProperties) ++
          (absentProperties flatMap (p => encodeAbsentProperty(m.df.getOWLNamedIndividual(individualRandomIRI), p)))
      case ExternalResource(v,infr) => Vector(m.df.getOWLObjectPropertyAssertionAxiom(objProp, sourceIndividual, individualFromInfrastructure(v,infr)))
//      case PolicyDocument(statements) => // TODO Again!
//        val policyRandomIRI = DLModelIRI.policyNodeIRI(stackSet.name)
//        createPolicyNode(policyRandomIRI) /*++
//          statements.flatMap(s => encodePropertyWithIRI
//              (m.df.getOWLNamedIndividual(policyRandomIRI), DLModelIRI.propertyTypeIRI("policydocument", "statement"), s))*/
//      case AllowStatement(_,_,_,_) => null // TODO
//      case DenyStatement(_,_,_,_) => null // TODO Continue from here and decide if you should move the instantiation from here to another class!
      case _ => Vector() // TODO
    }


    def createBlankNode(sourceIndividual: OWLIndividual, objProp: OWLObjectProperty, individualRandomIRI: IRI): Vector[OWLAxiom] = {
        range(objProp) match {
          case None =>
            val newNode = m.df.getOWLNamedIndividual(individualRandomIRI)
            addComment(newNode, AwsOntology.SubpropertyAnnotationComment)
            Vector(m.df.getOWLObjectPropertyAssertionAxiom(objProp,sourceIndividual,newNode)) ++
            Vector(m.df.getOWLDeclarationAxiom(newNode))
          case Some(c) =>
            val newNode = m.df.getOWLNamedIndividual(individualRandomIRI)
            addComment(newNode, AwsOntology.SubpropertyAnnotationComment)
            Vector(m.df.getOWLObjectPropertyAssertionAxiom(objProp,sourceIndividual,newNode)) ++
            Vector(m.df.getOWLClassAssertionAxiom(c,newNode))
        }
      }


//    def createPolicyNode(policyRandomIRI : IRI) : Vector[OWLAxiom] = {
//      val policyNode = m.df.getOWLNamedIndividual(ModelIRI.policyNodeIRI(stackSet.name))
//      addComment(policyNode, "policy")
//      Vector ( m.df.getOWLClassAssertionAxiom( classFromIRI(ModelIRI.policyDocIRI), policyNode ))
//    }


    def individualFromStackSet(name:String, stackSet: StackSet) : OWLIndividual
    = m.df.getOWLNamedIndividual(ModelIRI.resourceInstanceIRI(stackSet.name,name))

    def individualFromInfrastructure(name:String, infrastructure: Infrastructure) : OWLIndividual
    = m.df.getOWLNamedIndividual(ModelIRI.externalEntityIRI(infrastructure.name,name))


    def range(objProp: OWLObjectProperty) : Option[OWLClass] = {
      m.manager.getOntology(ModelIRI.resourceTerminologyIRI(resource.serviceType+resource.resourceType)).objectPropertyRangeAxioms(objProp).findFirst().toScala match {
        case Some(ax) => Some(ax.getRange.asOWLClass())
        case None => None
      }
    }

    def oProperty(name:String): Option[OWLObjectProperty] = {
      if (m.manager.getOntology(ModelIRI.resourceTerminologyIRI(resource.serviceType+resource.resourceType)).containsObjectPropertyInSignature(ModelIRI.propertyTypeIRI(resource.serviceType + resource.resourceType, name)))
        Some(m.df.getOWLObjectProperty(ModelIRI.propertyTypeIRI(resource.serviceType+resource.resourceType, name)))
      else None
    }

    def dProperty(name:String): Option[OWLDataProperty] = {
      if (m.manager.getOntology(ModelIRI.resourceTerminologyIRI(resource.serviceType+resource.resourceType )).containsDataPropertyInSignature(ModelIRI.propertyTypeIRI(resource.serviceType+resource.resourceType, name)))
        Some(m.df.getOWLDataProperty(ModelIRI.propertyTypeIRI(resource.serviceType+resource.resourceType, name)))
      else None
    }


//    def oPropFromIRI(iri:IRI) = m.df.getOWLObjectProperty(iri)


    val resourceInstanceIRI = ModelIRI.resourceInstanceIRI(stackSet.name, resource.resourceLogicalId)

    encodeAllGivenSubproperties( resourceInstanceIRI, resource.givenProperties ) ++
      encodeAllAbsentSuproperties( resourceInstanceIRI, resource.absentProperties )

  }


  private def classFromIRI(iri:IRI) = m.df.getOWLClass(iri)

  private def addComment (individual:OWLNamedIndividual, comment:String) = {
    val commentAxiom  = m.df.getOWLAnnotationAssertionAxiom( individual.getIRI , m.df.getRDFSComment(comment ) )
    m.manager.applyChange( new AddAxiom( m.ontology, commentAxiom ))
  }

}
