package aws.cfn.templates.formalization


sealed trait Node
sealed trait StackSetNode extends Node

  sealed trait GenericValueNode extends StackSetNode
    case object NoValue extends GenericValueNode
    sealed trait ValueNode[T] extends GenericValueNode
      final case class StringNode(value:String) extends ValueNode[String]
      final case class BooleanNode(value: Boolean) extends ValueNode[Boolean]
      final case class IntNode(value: Int) extends ValueNode[Int]
      final case class FloatNode(value: Float) extends ValueNode[Float]
      final case class DoubleNode(value : Double) extends  ValueNode[Double]
      final case class LongNode(value: Long) extends ValueNode[Long]
      final case class JsonNode(value: String) extends ValueNode[String]
      final case class CommaDelimitedListNode(value: String) extends ValueNode[String]
      final case class TimeStampNode(value: String) extends ValueNode[String]
      final case class ListNode[T<:Node](value: Vector[T]) extends GenericValueNode
      final case class MapNode[T<:Node](value: Map[String,T]) extends GenericValueNode


  sealed trait ObjectNode extends StackSetNode
    final case class ForeignResource(name:String) extends Node with ObjectNode

    final case class StackSetResource(resourceLogicalId: String,
                                      serviceType: String,
                                      resourceType : String,
                                      attributes : Map[String, GenericValueNode],
                                 ) extends ObjectNode
    {
      val value: String = resourceLogicalId
      var givenProperties: Map[String,Node] = Map()
      var absentProperties: Set[String] = Set()
      def apply(): StackSetResource = this
    }

    final case class Subproperty(givenProperties: Map[String,Node],
                                 absentProperties: Set[String]=Set()) extends ObjectNode
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
                                      r:(Boolean, Vector[Node]),
                                      c: Map[String,(AnyVal, AnyVal)] ) extends Statement

      final case class DenyStatement(p: (Boolean,Vector[Node]),
                                     a: (Boolean,Vector[String]),
                                     r:(Boolean, Vector[Node]),
                                     c: Map[String,(AnyVal, AnyVal)] ) extends Statement