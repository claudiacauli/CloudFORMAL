package aws.cfn.formalization


class StackSet(val name:String, val templates:Vector[Template])
class Template(val name:String, val inputParameters : Map[String,ValueNode])

trait CloudFormationNode

  trait ValueNode extends CloudFormationNode

    case class StringNode(value: (=> String)) extends ValueNode
    case class BooleanNode(value: (=> Boolean)) extends ValueNode
    case class IntNode(value: (=> Int)) extends ValueNode
    case class FloatNode(value: (=>Float)) extends ValueNode
    case class LongNode(value: (=>Long)) extends ValueNode
    case class JsonNode(value: (=>String)) extends ValueNode
    case class CommaDelimitedLisNode (value: (=>String)) extends ValueNode
    case class DateTimeNode (value: (=> String)) extends ValueNode

    trait IntrinsicFunction extends ValueNode

      case class Base64Function() extends IntrinsicFunction
      case class CidrFunction() extends IntrinsicFunction
      case class FindInMapFunction() extends IntrinsicFunction
      case class GetAttFunction() extends IntrinsicFunction
      case class GetAZsFunction() extends IntrinsicFunction
      case class ImportValueFunction() extends IntrinsicFunction
      case class JoinFunction() extends IntrinsicFunction
      case class SelectFunction() extends IntrinsicFunction
      case class SplitFunction() extends IntrinsicFunction
      case class SubFunction() extends IntrinsicFunction
      case class TransformFunction() extends IntrinsicFunction
      case class RefFunction() extends IntrinsicFunction

      trait ConditionFunction extends IntrinsicFunction
        case class IfFunction() extends ConditionFunction
        case class AndFunction() extends ConditionFunction
        case class OrFunction() extends ConditionFunction
        case class NotFunction() extends ConditionFunction
        case class EqualsFunction() extends ConditionFunction

  trait ObjectNode extends CloudFormationNode

    case class ResourceNode() extends ObjectNode
    case class SubpropertyNode() extends ObjectNode
    case class PolicyNode() extends ObjectNode



//
//sealed trait AttributeValue
//sealed trait SingleValue
//  sealed trait PrimitiveValue extends AttributeValue with SingleValue
//    case class StringValue(v:String)  extends PrimitiveValue
//    case class IntValue(v:Int)  extends PrimitiveValue
//    case class LongValue(v:Long)  extends PrimitiveValue
//    case class FloatValue(v:Float)  extends PrimitiveValue
//    case class BooleanValue(v:Boolean)  extends PrimitiveValue
//    case class DateTimeValue(v:String)  extends PrimitiveValue
//    case class JsonValue(v:String)  extends PrimitiveValue
//    case class CommaDelimitedListValue(v:String)  extends PrimitiveValue {
//      val asList : List[String] = v.split(',').toList
//    }
//  sealed trait NonPrimitiveValue extends SingleValue
//    case class Resource(name: String, props: List[GenericProperty], attrs: List[GenericAttribute]) extends NonPrimitiveValue
//    case class SubProperty(name: String, props: List[GenericProperty]) extends NonPrimitiveValue
//sealed trait MultiValue
//  sealed trait ListValue extends MultiValue
//    case class ListOfPrimitiveValues[T <: PrimitiveValue](values: List[T]) extends ListValue with AttributeValue
//    case class ListOfNonPrimitiveValues[T <: NonPrimitiveValue](values: List[T]) extends ListValue
//  case class MapValue[T <:SingleValue](map: Map[String,T]) extends MultiValue
//
//sealed trait GenericAttribute
//case class Attribute[T <: AttributeValue](name: String, value:T, doc:String) extends GenericAttribute
//sealed trait GenericProperty
//case class Property[T <: PrimitiveValue](name: String, value:T, req:Boolean, fun:Boolean,  doc:String) extends GenericProperty