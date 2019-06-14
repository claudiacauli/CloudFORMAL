package aws.cfn.mapping.templates

import java.nio.charset.StandardCharsets
import java.util.Base64

import aws.cfn.mapping.Specification
import com.typesafe.scalalogging.{Logger, StrictLogging}

import scala.language.postfixOps


private sealed trait IntrinsicFunction
  extends CloudFormationFunction


private final case class Base64Function()
  extends IntrinsicFunction
  with (StringNode => StringNode)
{
  def apply(s: StringNode): StringNode = {
    val base64string =
      Base64
        .getMimeEncoder
        .encodeToString(s.value.getBytes(StandardCharsets.UTF_8))
    StringNode (base64string)
  }
}





private final case class CidrFunction()
  extends IntrinsicFunction
  with ((StringNode,IntNode,IntNode) => Node)
{
  def apply(ipBlock: StringNode, count: IntNode, cidrBits: IntNode): Node = {
    println("Encountered Fn::Cidr. Currently not implemented. Returning NoValue")
    NoValue
  }
}





private final case class FindInMapFunction
(mappings: Map[String,Map[String,Either[String,Map[String,Any]]]])
  extends IntrinsicFunction
  with ((StringNode,StringNode,Option[StringNode]) => GenericValueNode)
{
  def apply(m: StringNode, k1: StringNode, k2: Option[StringNode]=None): GenericValueNode =
    k2 match {
      case None =>
        mappings(m.value.toLowerCase)(k1.value.toLowerCase) match {
          case Left(s)            => StringNode(s)
          case Right(m1)          => StringNode(m1.toString)
      }
      case Some(k) =>
        mappings(m.value.toLowerCase)(k1.value.toLowerCase) match {
          case Left(s)            => StringNode(s)
          case Right(m1) =>
            m1(k.value.toLowerCase) match {
              case f: Float       => FloatNode(f)
              case b: Boolean     => BooleanNode(b)
              case l: Long        => LongNode(l)
              case i: Int         => IntNode(i)
              case d: Double      => DoubleNode(d)
              case _              => StringNode(m1(k.value).asInstanceOf[String])
        }
      }
    }
}





private final case class GetAttFunction ( optRE: Option[Json2ResourceEncoder],
                                  tE: Json2TemplateEncoder)
  extends IntrinsicFunction
  with ((StringNode,StringNode) => Node)
  with StrictLogging
{
  def apply(resourceName:StringNode, attributeName:StringNode): Node =
  {
    if (tE.resources!=null && tE.resources.get(resourceName.value).isDefined) {
      if (attributeName.value == "arn") {
        val res = tE.resources(resourceName.value)
        updateResourceByPolicyMap(optRE, res)
        logger.debug(s"Fn::GetAtt(${resourceName.v},Arn) " +
          s"evaluated to StackSetResource $res." )
        res
      }
      else
        tE.resources(resourceName.value).attributes.get(attributeName.value) match {
          case Some(v)  => v
          case None     =>
//            println("No attribute with name " + attributeName + " found for resource " + tE.resources(resourceName.value))
//            println("Returning StringNode with concatenation of resource and attributename")
            logger.warn(s"Cannot evaluate Fn::GetAtt(${resourceName.v},${attributeName.v}). " +
              "If you wish to evaluate attributes add an \"Attributes\" block in addition to " +
              "the \"Properties\" block under the target resource.")
            StringNode(resourceName + "_attribute_" + attributeName)
        }
    }
    else NoValue
  }
}





// Availability zones might depend on the account and not only on the region!
private final case class GetAZsFunction() extends
  IntrinsicFunction
  with (StringNode => ListNode[StringNode])
{
  def apply(reg: StringNode): ListNode[StringNode] =
  {
    println("Need to implement a GetAZ Function")
    // TODO: This is an absolutely fake list
    ListNode[StringNode]( Vector(StringNode(reg+"a"), StringNode(reg+"b"), StringNode(reg+"c")) )
  }
}





private final case class ImportValueFunction( tE: Json2TemplateEncoder,
                                      optRE:Option[Json2ResourceEncoder])
  extends IntrinsicFunction
  with (StringNode => Node)
{
  def apply(importName: StringNode): Node =
  {
    def lookupOtherStackSets = {
      tE.ssE.iE.stackSetEncoders
        .find(_.outputsByExportName.get(importName.value).isDefined)
        .map(_.outputsByExportName(importName.value))
        .getOrElse(NoValue)
    }
    val res =
      tE.outputByLogicalId
        .getOrElse(importName.value,
          tE.outputByExportName
            .getOrElse(importName.value,
              lookupOtherStackSets))
    updateResourceByPolicyMap(optRE,res)
    res
  }
}





private final case class JoinFunction()
  extends IntrinsicFunction
  with ((StringNode, ListNode[Node]) => Node)
{
  def apply(delimiter: StringNode, segments: ListNode[Node]): Node =
  {
    StringNode(segments.value.map({
      case StringNode(v)                => v
      case IntNode(i)                   => i.toString
      case FloatNode(f)                 => f.toString
      case LongNode(f)                  => f.toString
      case DoubleNode(d)                => d.toString
      case ExternalResource(n,_)        => n
      case StackSetResource(id,_,_,_,_,_) => id
      case NoValue => ""
      }).mkString(delimiter.value))
  }
}





private final case class SelectFunction(optRE:Option[Json2ResourceEncoder])
  extends IntrinsicFunction
  with ((IntNode,ListNode[Node]) => Node)
{
  def apply(index:IntNode, list:ListNode[Node]) : Node =
  {
    if (index.value >= list.value.size)
      NoValue
    else {
      val item = list.value(index.value)
      updateResourceByPolicyMap(optRE,item)
      item
    }
  }
}





private final case class SplitFunction()
  extends IntrinsicFunction
    with ((StringNode,StringNode) => ListNode[StringNode])
{
  def apply(delimiter:StringNode, s:StringNode): ListNode[StringNode] =
    ListNode[StringNode](
      (s.value.split(delimiter.value) map (s => StringNode(s))).toVector)
}





private final case class SubFunction (optRE: Option[Json2ResourceEncoder],
                              tE: Json2TemplateEncoder)
  extends IntrinsicFunction
    with ((StringNode,Option[Map[String,String]]) => Node)
{
  def apply(str: StringNode, substitutionMap: Option[Map[String,String]]=None): Node =
  {
    var s = str.value.toLowerCase

    def sub(s:String,variable:String,value:String) : String  =
      s.replaceAll("\\$\\{" + variable + "\\}", value)
    def removeVarDelimiters() =
      s.replaceAll("\\$|\\{|\\}", "")

    // Round 1: Replace map variables
    if (substitutionMap.isDefined)
      s = substitutionMap.get.foldLeft (s) ((a, b) => sub(a,b._1,b._2))

    // Round 2: Replace parameters variables
    s = tE.parameters.foldLeft(s)((a, b) =>
      b._2 match {
        case StringNode(v) => sub(a,b._1,v)
        case _ => a
      })

    // Round 3: Replace attributes value
    if (s.contains(".arn}")) {
      s = s.split(".arn\\}").head.split("\\{").last
      if (tE.resources!=null && tE.resources.get(s).isDefined) {
        val res = tE.resources(s)
        updateResourceByPolicyMap(optRE,res)
        res
      }
      else NoValue
    }
    else    // Round 4: If there are still variables, get rid of special chars and hope they are resource names!
      StringNode ( removeVarDelimiters() )
  }
}





private final case class TransformFunction()
  extends IntrinsicFunction
{
  def apply() : Node = NoValue  //TODO
}





private final case class RefFunction(optRE: Option[Json2ResourceEncoder],
                             tE: Json2TemplateEncoder)
  extends IntrinsicFunction with (Node => Node)
{
  def apply(referredNode: Node): Node =
    referredNode match {
      case ssR: StackSetResource => ssR
      case eR:  ExternalResource => eR
      case StringNode(s) if s.startsWith(Specification.ArnHead) =>
        ArnFunction(optRE,tE)(s) match {
          case ListOfResources(v) if v.size==1 =>
            println("Here the Ref Function is returning only one resource!")
            updateResourceByArnMap(optRE,v.head,s)
            updateResourceByPolicyMap(optRE,v.head)
            v.head
          case l:ListOfResources => l
          case r =>
            updateResourceByArnMap(optRE,r,s)
            updateResourceByPolicyMap(optRE,r)
            r
        }
      case StringNode(s) if tE.parameters!=null && tE.parameters.get(s).isDefined
      => tE.parameters(s)
      case StringNode(s) if tE.resources!=null && tE.resources.get(s).isDefined
      =>  val res = tE.resources(s)
        updateResourceByPolicyMap(optRE,res)
        res
      case _  => NoValue
    }
}





private sealed trait ConditionFunction
  extends IntrinsicFunction


  private final case class IfFunction()
    extends ConditionFunction
      with ((BooleanNode,Node,Node) => Node)
  {
    def apply(c: BooleanNode, e1: Node, e2: Node): Node =
      if (c.value) e1 else e2
  }


private sealed trait BooleanFunction
  extends ConditionFunction


  private final case class AndFunction()
    extends BooleanFunction
      with ((BooleanNode,BooleanNode) => BooleanNode)
  {
    def apply(e1: BooleanNode, e2: BooleanNode) : BooleanNode =
      BooleanNode ( e1.value && e2.value )
  }


  private final case class OrFunction()
    extends BooleanFunction
      with ((BooleanNode,BooleanNode) => BooleanNode)
  {
    def apply(e1: BooleanNode, e2: BooleanNode): BooleanNode =
      BooleanNode ( e1.value || e2.value )
  }


  private final case class NotFunction()
    extends BooleanFunction
      with (BooleanNode => BooleanNode)
  {
    def apply(e: BooleanNode): BooleanNode =
      BooleanNode ( !e.value )
  }


  private final case class EqualsFunction()
    extends BooleanFunction
      with ((Node,Node) => BooleanNode)
  {
    def apply(e1: Node, e2: Node):BooleanNode =
      BooleanNode( e1 == e2 )
  }