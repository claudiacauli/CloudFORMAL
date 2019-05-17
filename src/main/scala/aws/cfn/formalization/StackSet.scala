package aws.cfn.formalization


class StackSet(val name:String) {

  var templates: Vector[Template] = Vector()
  var outputs: Map[String,Either[StackSetNode, AnyVal]] = Map()

  def addTemplates(templateInput: Vector[(String, Map[String,Any])] ): Unit =
    templates = templateInput map ( p => new Template(p._1, p._2) )

  def addOutputs (outs: Map[String, Either[StackSetNode, AnyVal]]) : Unit =
    outputs = outs


}




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