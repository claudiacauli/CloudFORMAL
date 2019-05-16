package aws.cfn.formalization

import java.nio.charset.StandardCharsets
import java.util.Base64


/*
 * Collection of all possible nodes in a CFn template
 */
sealed class CloudFormationNode {
  //def equals(other: CloudFormationNode): Boolean = this == other
}


  /*
   * Nodes that contains values of any type. Leaf nodes
   */
  sealed trait GenericValueNode extends CloudFormationNode

    sealed trait ValueNode[T] extends GenericValueNode {
      def value: T
      //def equals(other: ValueNode[T]): Boolean = this.value == other.value
    }

      final case class StringNode(value: String) extends ValueNode[String] {
        def apply(): String = value
      }

      final case class BooleanNode(value: Boolean) extends ValueNode[Boolean] {
        def apply(): Boolean = value
      }

      final case class IntNode(value: Int) extends ValueNode[Int] {
        def apply(): Int = value
      }

      final case class FloatNode(value: Float) extends ValueNode[Float] {
        def apply(): Float = value
      }

      final case class LongNode(value: Long) extends ValueNode[Long] {
        def apply(): Long = value
      }

      final case class JsonNode(value: String) extends ValueNode[String] {
        def apply(): String = value
      }

      final case class CommaDelimitedLisNode(value: String) extends ValueNode[String] {
        def apply(): String = value
      }

      final case class DateTimeNode(value: String) extends ValueNode[String] {
        def apply(): String = value
      }






  /*
   * Intrinsic functions! Evaluate to several different things...
   */
  sealed trait IntrinsicFunction extends CloudFormationNode {
    //def equals(other : IntrinsicFunction) : Boolean = this == other
  }

    final case class Arn(p: String, ss:StackSet, t:Template) extends IntrinsicFunction {
      def apply(): ResourceNode = null // TODO
    }

    final case class Base64Function(p: String) extends IntrinsicFunction {
      def apply(): String = Base64.getMimeEncoder.encodeToString(p.getBytes(StandardCharsets.UTF_8))
    }

    final case class CidrFunction(ipBlock: String, count: Int, cidrBits: Int) extends IntrinsicFunction {
      def apply(): String = ipBlock // TODO
    }

    final case class FindInMapFunction(m: String, k1: String, k2: String, t:Template) extends IntrinsicFunction {
      def apply(): Either[CloudFormationNode,AnyVal] = t.mappings(k1)(k2)
    }

    final case class GetAttFunction(res:String, attr:String, t:Template) extends IntrinsicFunction {
      def apply(): AnyVal = t.resources(res).attributes(attr)
    }

    // Availability zones might depend on the account and not only on the region!
    final case class GetAZsFunction(reg: String) extends IntrinsicFunction {
      def apply(): Vector[String] = Vector(reg+"a", reg+"b", reg+"c") // TODO This is an absolutely fake list
    }

    final case class ImportValueFunction(importName: String, ss:StackSet) extends IntrinsicFunction {
      implicit def apply() : AnyRef = ss.outputs(importName)
    }

    final case class JoinFunction(delimiter:String, values:Vector[String]) extends IntrinsicFunction {
      def apply(): String = values.mkString(delimiter)
    }

    final case class SelectFunction(index:Int, list:Vector[Any]) extends IntrinsicFunction {
      def apply(): Any = list(index)
    }

    final case class SplitFunction(delimiter:String, value:String) extends IntrinsicFunction {
      def apply(): Vector[String] = value.split(delimiter).toVector
    }

    final case class SubFunction(str:String, subMap:Option[Map[String,String]], t:Template) extends IntrinsicFunction {
      def apply(): Any = subMap match {
        case None => if (str.contains(".")) GetAttFunction(str.split(".")(0), str.split(".")(1), t)() else RefFunction(str, t)()
        case Some(m) => m.foldLeft(str)((a, b) => a.replaceAll("\\$\\{" + b._1 + "\\}" ,b._2))
      }
    }

    final case class TransformFunction() extends IntrinsicFunction {
      def apply() : Any = null // TODO
    }

    final case class RefFunction(p: String, t:Template) extends IntrinsicFunction {
      def apply(): Either[ResourceNode, Any] = null // TODO
    }






      sealed trait ConditionFunction extends IntrinsicFunction

        final case class IfFunction() extends ConditionFunction {
          def apply(c: Boolean, e1: GenericValueNode, e2: GenericValueNode): GenericValueNode = if (c) e1 else e2
        }


      /*
       * Functions that evaluates to booleans
       */
      sealed trait BooleanFunction extends ConditionFunction

        final case class AndFunction() extends BooleanFunction {
          def apply(e1: Boolean, e2: Boolean): Boolean = e1 && e2
        }

        final case class OrFunction() extends BooleanFunction {
          def apply(e1: Boolean, e2: Boolean): Boolean = e1 || e2
        }

        final case class NotFunction() extends BooleanFunction {
          def apply(e: Boolean): Boolean = !e
        }

        final case class EqualsFunction() extends BooleanFunction {
          def apply(e1: AnyVal, e2: AnyVal):Boolean = e1 == e2
        }






  /*
   * Object Nodes, extending CloudFormation Nodes
   */
  sealed trait ObjectNode extends CloudFormationNode {
    //def equals(other: ObjectNode): Boolean = this == other
  }

    final case class ResourceNode(resourceLogicalId: String,
                            condition: Either[Boolean,BooleanFunction],
                            attributes : Map[String, AnyVal],
                            properties: Map[String,CloudFormationNode]) extends ObjectNode
    {
      def apply(): ResourceNode = this
    }

    final case class SubpropertyNode(properties: Map[String, CloudFormationNode]) extends ObjectNode
    {
      def apply(): SubpropertyNode = this
    }

    final case class PolicyNode() extends ObjectNode
    {
      def apply(): PolicyNode = this
    }
