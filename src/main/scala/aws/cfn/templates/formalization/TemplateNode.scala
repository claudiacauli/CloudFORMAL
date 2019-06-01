package aws.cfn.templates.formalization

import java.nio.charset.StandardCharsets
import java.util.Base64

import aws.cfn.templates.encoding.Json2StackSetEncoder


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


  sealed trait IntrinsicFunction extends StackSetNode

    final case class ArnFunction(resources:Map[String,StackSetResource],
                                 parameters:Map[String,Node],
                                 arnString: String,
                                 arnsMap:Map[String,Node]) extends IntrinsicFunction {
      def apply(): Node = {
        val evalArnStringNode = SubFunction(resources, parameters, StringNode(arnString)) ()
        new Arn(evalArnStringNode.value).resourceFromArn()
      }
    }

    final case class Base64Function(string: StringNode) extends IntrinsicFunction {
      def apply(): StringNode =
        StringNode ( Base64.getMimeEncoder.encodeToString(string.value.getBytes(StandardCharsets.UTF_8)) )
    }

    final case class CidrFunction(ipBlock: StringNode, count: IntNode, cidrBits: IntNode) extends IntrinsicFunction {
      def apply(): StringNode = ipBlock // TODO
    }

    final case class FindInMapFunction(mappings: Map[String,Map[String,Either[String,Map[String,Any]]]],
                                       m: StringNode,
                                       k1: StringNode,
                                       k2: StringNode = null) extends IntrinsicFunction {
      def apply() : GenericValueNode = {
        if ( k2==null ) {
          mappings(m.value)(k1.value) match {
            case Left(s) => StringNode(s)
            case Right(m1) => StringNode(m1.toString)
          }
        }
        else mappings(m.value)(k1.value) match {
          case Left(s) => StringNode(s)
          case Right(m1) => m1(k2.value) match {
            case float: Float => FloatNode(m1(k2.value).asInstanceOf[Float])
            case bool: Boolean => BooleanNode(m1(k2.value).asInstanceOf[Boolean])
            case _ => StringNode(m1(k2.value).asInstanceOf[String])
          }
        }
    }
    }


    final case class GetAttFunction(res:StringNode,
                                    attr:StringNode,
                                    resources:Map[String,StackSetResource]) extends IntrinsicFunction {
      def apply(): Node = {
        if (attr.value.equals("arn") && resources!=null && resources.get(res.value).isDefined)
          resources(res.value)
        else NoValue
        //resources(res.value).attributes(attr.value) //TODO!
      }
    }

    // Availability zones might depend on the account and not only on the region!
    final case class GetAZsFunction(reg: StringNode) extends IntrinsicFunction {
      def apply(): ListNode[StringNode] = {
        // TODO: This is an absolutely fake list
        ListNode[StringNode]( Vector(StringNode(reg+"a"), StringNode(reg+"b"), StringNode(reg+"c")) )
      }
    }

    final case class ImportValueFunction( importName: StringNode,
                                          outputsByExportName: Map[String,Node],
                                          outputsByLogicalId: Map[String,Node]) extends IntrinsicFunction {
      implicit def apply(): Node = {
        outputsByLogicalId.getOrElse(importName.value, outputsByExportName.getOrElse(importName.value, NoValue))
      }

    }

    final case class JoinFunction(delimiter:StringNode,
                                  values:ListNode[StringNode]) extends IntrinsicFunction {
      def apply(): StringNode =
        StringNode ( (values.value map ( i => i.value )).mkString(delimiter.value) )
    }

    final case class SelectFunction(index:IntNode,
                                    list:ListNode[Node]) extends IntrinsicFunction {
      def apply(): Node =
        if (index.value >= list.value.size)
          NoValue
        else list.value(index.value)
    }

    final case class SplitFunction(delimiter:StringNode, v:StringNode) extends IntrinsicFunction {
      def apply(): ListNode[StringNode] = {
        ListNode[StringNode]( (v.value.split(delimiter.value) map (s => StringNode(s))).toVector )
      }
    }

    final case class SubFunction(resources:Map[String,StackSetResource],
                                 parameters:Map[String,Node],
                                 str:StringNode,
                                 subMap:Option[Map[String,String]] = None) extends IntrinsicFunction {
      def apply():StringNode = {
        var tempString = str.value.toLowerCase
        if (subMap.isDefined) { // FIRST ROUND: Replace what you find in the map
          tempString = subMap.get.foldLeft(tempString)((a, b) => a.replaceAll("\\$\\{" + b._1 + "\\}", b._2))
        }
        tempString = parameters.foldLeft(tempString)((a, b) => {    // SECOND ROUND: Replace what you find in parameters
          if (b._2.isInstanceOf[StringNode]) {
            a.replaceAll("\\$\\{" + b._1 + "\\}" ,b._2.asInstanceOf[StringNode].value)
          }
          else
            a
          })
        if (tempString.contains("."))  // THIRD ROUND: Replace what can be an attribute
          GetAttFunction(StringNode(tempString.split("\\.")(0)), StringNode(tempString.split("\\.")(1)), resources)()
        StringNode ( tempString.replaceAll("\\$|\\{|\\}", "") )     // FOURTH ROUND: IF THERE are still variables just get rid of the delimiter and hope it was a local resource name!
      }
    }

    final case class TransformFunction() extends IntrinsicFunction {
      def apply() : Node = null // TODO
    }

    final case class RefFunction(n: Node,
                                 resources:Map[String,StackSetResource],
                                 parameters:Map[String,Node],
                                 ssE:Json2StackSetEncoder) extends IntrinsicFunction {
      def apply(): Node = {
          n match {
            case str: StringNode => {
              if (str.value.startsWith("arn:")){
                val referredNode = ArnFunction(resources,parameters, str.value, ssE.resourceByArn)()
                if (!ssE.resourceByArn.values.toVector.contains(referredNode))
                  ssE.foreignResourcesByArn = ssE.foreignResourcesByArn ++ Map(str.value -> referredNode.asInstanceOf[ForeignResource])
                referredNode
              }
              else if (parameters!=null && parameters.get(str.value).isDefined){
                parameters(str.value)
              }
              else if (resources!=null && resources.get(str.value).isDefined)
                resources(str.value)
              else NoValue
            }
            case node: StackSetResource => node
            case node: ForeignResource => node
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

    final case class ForeignResource(name:String) extends Node with ObjectNode

    final case class StackSetResource(resourceLogicalId: String,
                                      serviceType: String,
                                      resourceType : String,
                                      attributes : Map[String, GenericValueNode],
                                 ) extends ObjectNode
    {
      val value = resourceLogicalId
      var givenProperties: Map[String,Node] = Map()
      var absentProperties: Set[String] = Set()
      def apply(): StackSetResource = this
    }

    final case class Subproperty(givenProperties: Map[String,Node],
                                 absentProperties: Set[String]=Set()) extends ObjectNode
    {
      def apply(): Subproperty = this
    }

    final case class Policy(statements: Vector[Statement]) extends ObjectNode
    {
      def apply(): Policy = this
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