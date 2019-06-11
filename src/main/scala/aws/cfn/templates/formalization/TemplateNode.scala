package aws.cfn.templates.formalization


sealed trait Node
sealed trait StackSetNode extends Node

  sealed trait GenericValueNode extends StackSetNode
    case object NoValue extends GenericValueNode
    sealed class ValueNode[T](value: T) extends GenericValueNode {
      override def toString: String = value.toString
    }
      final case class StringNode(value:String)     extends ValueNode[String](value)
      final case class BooleanNode(value: Boolean)  extends ValueNode[Boolean](value)
      final case class IntNode(value: Int)          extends ValueNode[Int](value)
      final case class FloatNode(value: Float)      extends ValueNode[Float](value)
      final case class DoubleNode(value : Double)   extends  ValueNode[Double](value)
      final case class LongNode(value: Long)        extends ValueNode[Long](value)
      final case class JsonNode(value: String)      extends ValueNode[String](value)
      final case class CommaDelimitedListNode(value: String) extends ValueNode[String](value)
      final case class TimeStampNode(value: String)   extends ValueNode[String](value)
      final case class ListNode[T<:Node](value: Vector[T])  extends GenericValueNode{
        override def toString: String = value.toString()
      }
      final case class MapNode[T<:Node](value: Map[String,T]) extends GenericValueNode{
        override def toString: String = value.toString()
      }


  sealed trait ObjectNode extends StackSetNode

    sealed trait Entity extends ObjectNode

      case object Public extends Entity {
        override def toString : String = "Public"
      }

      final case class ExternalEntity(name:String) extends Node with Entity

      final case class Resource(resourceLogicalId : String,
                                serviceType : String,
                                resourceType : String,
                                attributes : Map[String, GenericValueNode]
                                   ) extends Entity
      {
        val value: String = resourceLogicalId
        var resourceName : String = resourceLogicalId
        var givenProperties: Map[String,Node] = Map()
        var absentProperties: Set[String] = Set()
        def apply(): Resource = this

        override def toString: String = {
         serviceType + "::" + resourceType + "(" + resourceLogicalId + ")"
        }
      }

    final case class ListOfObjectNodes(nodes:Vector[Node]) extends ObjectNode

    final case class Subproperty(givenProperties  : Map[String,Node],
                                 absentProperties : Set[String] = Set()) extends ObjectNode
    {
      def apply(): Subproperty = this
    }

    final case class PolicyDocument(statements: Vector[Statement]) extends ObjectNode
    {
      def apply(): PolicyDocument = this
    }



    sealed class Statement (p: (Boolean,Vector[Node]),
                            a: (Boolean,Vector[String]),
                            r: (Boolean, Vector[Node]),
                            hasC: Boolean )  extends ObjectNode {

      def list(l:Vector[Any]) = {
        l.foldLeft("")((a,b)=>a+b+" ")
      }

      def pretty(a: (Boolean,Vector[Any])) = {
        if (!a._1) " NOT { " + list(a._2) + " }" else " { " + list(a._2) + " }"
      }

      def pretty(hasC: Boolean): String = {
        if (hasC) "\n with a condition."
        else "\n with NO condition."
      }

    }

      final case class AllowStatement(p: (Boolean,Vector[Node]),
                                      a: (Boolean,Vector[String]),
                                      r: (Boolean, Vector[Node]),
                                      hasC: Boolean ) extends Statement(p,a,r,hasC){
        override def toString: String = {
          "Allows \n principals " + pretty(p) + "\n to perform actions " + pretty(a) + "\n on resources " + pretty(r) + pretty(hasC)
        }
      }

      final case class DenyStatement(p: (Boolean,Vector[Node]),
                                     a: (Boolean,Vector[String]),
                                     r: (Boolean, Vector[Node]),
                                     hasC: Boolean ) extends Statement(p,a,r,hasC){
        override def toString: String = {
          "Denies " + pretty(p) + " to perform actions " + pretty(a) + " on resources " + pretty(r) + pretty(hasC)
        }
      }