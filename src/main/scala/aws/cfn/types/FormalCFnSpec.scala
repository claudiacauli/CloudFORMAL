package aws.cfn.types

class ResourceSpecification(val name:String, val resType:ResourceValueType, val subPropTypes: Vector[SubPropertyValueType]) {
  override def toString: String = name ++ "_ResourceSpecification (\n" ++
    "\t" + resType.toString + "\n" ++
    "\t" + subPropTypes.toString + "\n)"
}

sealed trait PropertyValueType
sealed trait AttributeValueType

sealed trait SingleValueType extends PropertyValueType
  sealed trait PrimitiveValueType extends SingleValueType with AttributeValueType
    case object StringValueType extends PrimitiveValueType
    case object CommaDelimitedListValueType extends PrimitiveValueType
    case object IntValueType extends PrimitiveValueType
    case object FloatValueType extends PrimitiveValueType
    case object LongValueType extends PrimitiveValueType
    case object BooleanValueType extends PrimitiveValueType
    case object DateTimeValueType extends PrimitiveValueType
    case object JsonValueType extends PrimitiveValueType

  sealed trait NonPrimitiveValueType extends SingleValueType {
    val name : String
  }

    case class SubPropertyValueType(name: String) extends NonPrimitiveValueType {
      var props : Vector[GenericPropertyType] = Vector()
      def addPropertyList(l:Vector[GenericPropertyType]): Unit = {
        this.props = l
      }
    }

    case class ResourceValueType(name: String) extends NonPrimitiveValueType {
      var attrs : Vector[GenericAttributeType] = Vector()
      var props : Vector[GenericPropertyType] = Vector()
      def addAttributeList(l:Vector[GenericAttributeType]): Unit = {
        this.attrs = l
      }
      def addPropertyList(l:Vector[GenericPropertyType]): Unit = {
        this.props = l
      }

    }


sealed trait MultiValueType extends PropertyValueType
  sealed trait ListValueType extends MultiValueType
    case class ListOfPrimitiveValueType[T <: PrimitiveValueType]() extends ListValueType with AttributeValueType
    case class ListOfNonPrimitiveValueType[T <: NonPrimitiveValueType]() extends ListValueType
  case class MapValueType[T <: SingleValueType]() extends MultiValueType



sealed trait GenericAttributeOrPropertyType
sealed trait GenericAttributeType extends GenericAttributeOrPropertyType
  case class StringAttribute(name: String, domain: ResourceValueType) extends GenericAttributeType
  case class IntAttribute(name: String, domain: ResourceValueType) extends GenericAttributeType
  case class FloatAttribute(name: String, domain: ResourceValueType) extends GenericAttributeType
  case class LongAttribute(name: String, domain: ResourceValueType) extends GenericAttributeType
  case class CommaDelimitedListAttribute(name: String, domain: ResourceValueType) extends GenericAttributeType
  case class BooleanAttribute(name: String, domain: ResourceValueType) extends GenericAttributeType
  case class DateTimeAttribute(name: String, domain: ResourceValueType) extends GenericAttributeType
  case class JsonAttribute(name:String, domain: ResourceValueType) extends GenericAttributeType
  case class ListOfPrimitiveAttribute(primitiveType: String, name:String, domain: ResourceValueType) extends GenericAttributeType

sealed trait GenericPropertyType extends GenericAttributeOrPropertyType
  case class StringProperty(name: String, domain: NonPrimitiveValueType, req:Boolean) extends GenericPropertyType
  case class IntProperty(name: String, domain: NonPrimitiveValueType, req:Boolean) extends GenericPropertyType
  case class FloatProperty(name: String, domain: NonPrimitiveValueType, req:Boolean) extends GenericPropertyType
  case class LongProperty(name: String, domain: NonPrimitiveValueType, req:Boolean) extends GenericPropertyType
  case class CommaDelimitedListProperty(name: String, domain: NonPrimitiveValueType, req:Boolean) extends GenericPropertyType
  case class BooleanProperty(name: String, domain: NonPrimitiveValueType, req:Boolean) extends GenericPropertyType
  case class DateTimeProperty(name: String, domain: NonPrimitiveValueType, req:Boolean) extends GenericPropertyType
  case class JsonProperty(name:String, domain: NonPrimitiveValueType, req:Boolean) extends GenericPropertyType
  case class ListOfPrimitiveProperty(primitiveType: String, name:String, domain: NonPrimitiveValueType, req:Boolean) extends GenericPropertyType
  case class ListOfNonPrimitiveProperty(nonPrimitiveType: String, name:String, domain: NonPrimitiveValueType, req:Boolean) extends GenericPropertyType
  case class MapOfPrimitiveProperty(primitiveType: String, name:String, domain:NonPrimitiveValueType, req:Boolean) extends GenericPropertyType
  case class MapOfNonPrimitiveProperty(nonPrimitiveType: String, name: String, domain: NonPrimitiveValueType, req:Boolean) extends GenericPropertyType
  case class SubpropertyProperty(subpropTypeName: String, name:String, domain: NonPrimitiveValueType, req:Boolean) extends GenericPropertyType
  case class ResourceProperty(resSpecName: String, resName: String, name: String, domain: NonPrimitiveValueType, req:Boolean) extends GenericPropertyType
  case class PolicyProperty(name:String, domain:NonPrimitiveValueType, req:Boolean) extends GenericPropertyType

class Action(val name : String, val resource:String)
class ServiceActions(val serviceName : String, val actions: Vector[Action])
