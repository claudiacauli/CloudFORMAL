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

    final case class ForeignResource(name:String) extends Node with ObjectNode

    final case class StackSetResource(resourceLogicalId : String,
                                      serviceType : String,
                                      resourceType : String,
                                      attributes : Map[String, GenericValueNode],
                                 ) extends ObjectNode
    {
      val value: String = resourceLogicalId
      var givenProperties: Map[String,Node] = Map()
      var absentProperties: Set[String] = Set()
      def apply(): StackSetResource = this

      override def toString: String = {
       serviceType + "::" + resourceType + "(" + resourceLogicalId + ")"
      }
    }

    final case class Subproperty(givenProperties  : Map[String,Node],
                                 absentProperties : Set[String]=Set()) extends ObjectNode
    {
      def apply(): Subproperty = this
    }

    final case class PolicyDocument(statements: Vector[Statement]) extends ObjectNode
    {
      def apply(): PolicyDocument = this
    }

    sealed trait Statement extends ObjectNode

      final case class AllowStatement(p: (Boolean,Vector[Node]),
                                      a: (Boolean,Vector[String]),
                                      r: (Boolean, Vector[Node]),
                                      c: Map[String,(AnyVal, AnyVal)] ) extends Statement

      final case class DenyStatement(p: (Boolean,Vector[Node]),
                                     a: (Boolean,Vector[String]),
                                     r: (Boolean, Vector[Node]),
                                     c: Map[String,(AnyVal, AnyVal)] ) extends Statement