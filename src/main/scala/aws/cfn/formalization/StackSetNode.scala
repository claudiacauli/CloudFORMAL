package aws.cfn.formalization

import java.nio.charset.StandardCharsets
import java.util.Base64

import argonaut.{DecodeJson, EncodeJson, Json}


sealed trait Node

/*
* Collection of all possible nodes in a CFn stackSet
*/
final class ForeignNode(name:String) extends Node

/*
 * Collection of all possible nodes in a CFn stackSet
 */
sealed trait StackSetNode extends Node


  /*
   * Nodes that contains values of any type. Leaf nodes
   */
  sealed trait GenericValueNode extends StackSetNode

    case object NoValue extends GenericValueNode

    sealed trait ValueNode[T] extends GenericValueNode

      final case class StringNode(value:String) extends ValueNode[String] {
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

      final case class CommaDelimitedListNode(value: String) extends ValueNode[String] {
        def apply(): String = value
      }

      final case class DateTimeNode(value: String) extends ValueNode[String] {
        def apply(): String = value
      }






  /*
   * Intrinsic functions! Evaluate to several different things...
   */
  sealed trait IntrinsicFunction extends StackSetNode

    final case class Arn(p: String, arnsMap:Map[String,Node]) extends IntrinsicFunction {
      def apply(): Node = arnsMap.getOrElse(p,new ForeignNode(p))
    }

    final case class Base64Function(p: String) extends IntrinsicFunction {
      def apply(): String = Base64.getMimeEncoder.encodeToString(p.getBytes(StandardCharsets.UTF_8))
    }

    final case class CidrFunction(ipBlock: String, count: Int, cidrBits: Int) extends IntrinsicFunction {
      def apply(): String = ipBlock // TODO
    }

    final case class FindInMapFunction(mappings: Map[String,Json], m: String, k1: String, k2: String = null) extends IntrinsicFunction {
      def apply() : String =
        if ( k2==null ) DecodeJson.StringDecodeJson.decodeJson( mappings(m).field(k1).get ).toOption.get
        else DecodeJson.StringDecodeJson.decodeJson( mappings(m).field(k1).get.field(k2).get ).toOption.get
    }

    final case class GetAttFunction(res:String, attr:String, resources:Json) extends IntrinsicFunction {
      def apply(): String = DecodeJson.StringDecodeJson.decodeJson( resources.field(res).get.field("Attributes").get.field(attr).get ).toOption.get
    }

    // Availability zones might depend on the account and not only on the region!
    final case class GetAZsFunction(reg: String) extends IntrinsicFunction {
      def apply(): Vector[String] = Vector(reg+"a", reg+"b", reg+"c") // TODO This is an absolutely fake list
    }

    final case class ImportValueFunction( importName: String, outputsByExportName: Map[String,Any], outputsByLogicalId: Map[String,Any]) extends IntrinsicFunction {
      implicit def apply(): Any = outputsByLogicalId.getOrElse(importName, outputsByExportName.getOrElse(importName, NoValue))
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

    final case class SubFunction(resources:Json, parameters:Map[String,Any], str:String, subMap:Option[Map[String,String]] = None) extends IntrinsicFunction {
      def apply(): Any = subMap match {
        case None => if (str.contains(".")) GetAttFunction(str.split(".")(0), str.split(".")(1), resources)() else RefFunction(str, resources, parameters)()
        case Some(m) => m.foldLeft(str)((a, b) => a.replaceAll("\\$\\{" + b._1 + "\\}" ,b._2))
      }
    }

    final case class TransformFunction() extends IntrinsicFunction {
      def apply() : Any = null // TODO
    }

    final case class RefFunction(p: String, resources:Map[String,ResourceNode], parameters:Map[String,Any]) extends IntrinsicFunction {
      def apply(): Any = parameters.getOrElse(p, resources(p))
    }

    final case class RefFunction(n: Node, resources:Map[String,ResourceNode], parameters:Map[String,Any]) extends IntrinsicFunction {
      def apply(): Node = n
    }






      sealed trait ConditionFunction extends IntrinsicFunction

        final case class IfFunction(c: Boolean, e1: GenericValueNode, e2: GenericValueNode) extends ConditionFunction {
          def apply(): GenericValueNode = if (c) e1 else e2
        }


      /*
       * Functions that evaluates to booleans
       */
      sealed trait BooleanFunction extends ConditionFunction

        final case class AndFunction(e1: Boolean, e2: Boolean) extends BooleanFunction {
          def apply(): Boolean = e1 && e2
        }

        final case class OrFunction(e1: Boolean, e2: Boolean) extends BooleanFunction {
          def apply(): Boolean = e1 || e2
        }

        final case class NotFunction(e: Boolean) extends BooleanFunction {
          def apply(): Boolean = !e
        }

        final case class EqualsFunction(e1: AnyVal, e2: AnyVal) extends BooleanFunction {
          def apply():Boolean = e1 == e2
        }






  /*
   * Object Nodes, extending CloudFormation Nodes
   */
  sealed trait ObjectNode extends StackSetNode

    final case class ResourceNode(resourceLogicalId: String,
                                  serviceType: String,
                                  resourceType : String,
                                  attributes : Map[String, AnyVal],
                                  givenProperties: Map[String,StackSetNode],
                                  absentProperties: Vector[String]
                                 ) extends ObjectNode
    {
      def apply(): ResourceNode = this
    }

    final case class SubpropertyNode(givenProperties: Map[String, StackSetNode],
                                     absentProperties: Vector[String]) extends ObjectNode
    {
      def apply(): SubpropertyNode = this
    }

    final case class PolicyNode(statements: Vector[Statement]) extends ObjectNode
    {
      def apply(): PolicyNode = this
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