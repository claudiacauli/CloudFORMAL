package aws.cfn.formalization

import java.nio.charset.StandardCharsets
import java.util.Base64
import aws.cfn.encoding.template.Json2StackSetEncoder


sealed trait Node

/*
* Collection of all possible nodes in a CFn stackSet
*/

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

      final case class ListNode[T<:Node](value: Vector[T]) extends GenericValueNode {
        def apply(): Vector[T] = value
      }

      final case class MapNode[T<:Node](value: Map[String,T]) extends GenericValueNode {
        def apply():Map[String,T] = value
      }




  /*
   * Intrinsic functions! Evaluate to several different things...
   */
  sealed trait IntrinsicFunction extends StackSetNode

    final case class Arn(arnString: StringNode, arnsMap:Map[String,Node]) extends IntrinsicFunction {
      def apply(): Node = arnsMap.getOrElse(arnString.value, ForeignNode(arnString.value))
    }

    final case class Base64Function(string: StringNode) extends IntrinsicFunction {
      def apply(): StringNode = StringNode ( Base64.getMimeEncoder.encodeToString(string.value.getBytes(StandardCharsets.UTF_8)) )
    }

    final case class CidrFunction(ipBlock: StringNode, count: IntNode, cidrBits: IntNode) extends IntrinsicFunction {
      def apply(): StringNode = ipBlock // TODO
    }

    final case class FindInMapFunction(mappings: Map[String,Map[String,Either[String,Map[String,String]]]], m: StringNode, k1: StringNode, k2: StringNode = null) extends IntrinsicFunction {
      def apply() : StringNode = {
        if ( k2==null ) {
          mappings(m.value)(k1.value) match {
            case Left(s) => StringNode(s)
            case Right(m1) => StringNode(m1.toString)
          }
        }
        else mappings(m.value)(k1.value) match {
          case Left(s) => StringNode(s)
          case Right(m1) => StringNode(m1(k2.value))
        }
    }
    }


    final case class GetAttFunction(res:StringNode, attr:StringNode, resources:Map[String,ResourceNode]) extends IntrinsicFunction {
      def apply(): Node = {
        if (attr.value.equals("arn"))
          resources(res.value)
        else StringNode("")
        //resources(res.value).attributes(attr.value) //TODO!
      }
    }

    // Availability zones might depend on the account and not only on the region!
    final case class GetAZsFunction(reg: StringNode) extends IntrinsicFunction {
      def apply(): ListNode[StringNode] = ListNode[StringNode]( Vector(StringNode(reg+"a"), StringNode(reg+"b"), StringNode(reg+"c")) )// TODO This is an absolutely fake list
    }

    final case class ImportValueFunction( importName: StringNode, outputsByExportName: Map[String,Node], outputsByLogicalId: Map[String,Node]) extends IntrinsicFunction {
      implicit def apply(): Node = outputsByLogicalId.getOrElse(importName.value, outputsByExportName.getOrElse(importName.value, NoValue))
    }

    final case class JoinFunction(delimiter:StringNode, values:ListNode[StringNode]) extends IntrinsicFunction {
      def apply(): StringNode = StringNode( ((values.value) map (i => i.value)).mkString(delimiter.value) )
    }

    final case class SelectFunction(index:IntNode, list:ListNode[Node]) extends IntrinsicFunction {
      def apply(): Node = list.value(index.value)
    }

    final case class SplitFunction(delimiter:StringNode, v:StringNode) extends IntrinsicFunction {
      def apply(): ListNode[StringNode] = ListNode[StringNode]( (v.value.split(delimiter.value) map (s => StringNode(s))).toVector )
    }

    final case class SubFunction(resources:Map[String,ResourceNode], parameters:Map[String,Node], str:StringNode, subMap:Option[Map[String,String]] = None) extends IntrinsicFunction {
      def apply(): Node = {
        subMap match {
        case None =>
          if (str.value.contains("."))
            GetAttFunction(StringNode(str.value.split(".")(0)), StringNode(str.value.split(".")(1)), resources)()
          else
            StringNode ( parameters.foldLeft(str.value)((a, b) => a.replaceAll("\\$\\{" + b._1 + "\\}" ,b._2.asInstanceOf[StringNode].value)) )
//          else
//            RefFunction(str, resources, parameters)()
        case Some(m) => {
          StringNode ( m.foldLeft(str.value)((a, b) => a.replaceAll("\\$\\{" + b._1 + "\\}" ,b._2.asInstanceOf[StringNode].value)) )
        }
      }
      }
    }

    final case class TransformFunction() extends IntrinsicFunction {
      def apply() : Node = null // TODO
    }

    final case class RefFunction(n: Node, resources:Map[String,ResourceNode], parameters:Map[String,Node], ssE:Json2StackSetEncoder) extends IntrinsicFunction {
      def apply(): Node = {
          n match {
            case str: StringNode => {
              if (str.value.startsWith("arn:")){
                val referredNode = Arn(StringNode(str.value), ssE.resourceByArn)()
                if (!ssE.resourceByArn.values.toVector.contains(referredNode))
                  ssE.foreignNodesByArn = ssE.foreignNodesByArn ++ Map(str.value -> referredNode.asInstanceOf[ForeignNode])
                referredNode
              }
              else if (resources.get(str.value).isDefined)
                resources(str.value)
              else if (parameters.get(str.value).isDefined)
                parameters(str.value)
              else NoValue
            }
            case node: ResourceNode => node
            case node: ForeignNode => node
            case _ => NoValue //"Referred field NOT FOUND: Not a resource, nor parameter, nor arns for Resource or ForeignNode"
          }
      }
    }






      sealed trait ConditionFunction extends IntrinsicFunction

        final case class IfFunction(c: BooleanNode, e1: Node, e2: Node) extends ConditionFunction {
          def apply(): Node = if (c.value) e1 else e2
        }


      /*
       * Functions that evaluates to booleans
       */
      sealed trait BooleanFunction extends ConditionFunction

        final case class AndFunction(e1: BooleanNode, e2: BooleanNode) extends BooleanFunction {
          def apply(): BooleanNode = BooleanNode ( e1.value && e2.value )
        }

        final case class OrFunction(e1: BooleanNode, e2: BooleanNode) extends BooleanFunction {
          def apply(): BooleanNode = BooleanNode ( e1.value || e2.value )
        }

        final case class NotFunction(e: BooleanNode) extends BooleanFunction {
          def apply(): BooleanNode = BooleanNode ( !e.value )
        }

        final case class EqualsFunction(e1: Node, e2: Node) extends BooleanFunction {
          def apply():BooleanNode = BooleanNode( e1 == e2 )
        }






  /*
   * Object Nodes, extending CloudFormation Nodes
   */
  sealed trait ObjectNode extends StackSetNode

    final case class ForeignNode(name:String) extends Node with ObjectNode

    final case class ResourceNode(resourceLogicalId: String,
                                  serviceType: String,
                                  resourceType : String,
                                  attributes : Map[String, GenericValueNode],
                                 ) extends ObjectNode
    {
      var givenProperties: Map[String,Node] = Map()
      var absentProperties: Set[String] = Set()
      def apply(): ResourceNode = this
    }

    final case class SubpropertyNode( givenProperties: Map[String,Node],
                                      absentProperties: Set[String]) extends ObjectNode
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