package aws.cfn.templates.formalization

import java.nio.charset.StandardCharsets
import java.util.Base64
import aws.cfn.templates.encoding.Json2StackSetEncoder

import scala.language.postfixOps



sealed trait IntrinsicFunction





final case class ArnFunction( resources   :Map[String,StackSetResource],
                              parameters  :Map[String,Node],
                              arnsMap     :Map[String,Node] ) extends IntrinsicFunction
{
  def apply(arnString: String) : Node =
  {
    val evaluatedArnStringNode = SubFunction(resources, parameters) (StringNode(arnString))
    new Arn(evaluatedArnStringNode.value).resourceFromArn()
  }
}





final case class Base64Function() extends IntrinsicFunction
{
  def apply(s: StringNode): StringNode = {
    val base64string = Base64.getMimeEncoder.encodeToString(s.value.getBytes(StandardCharsets.UTF_8))
    StringNode (base64string)
  }
}





final case class CidrFunction() extends IntrinsicFunction
{
  def apply(ipBlock: StringNode, count: IntNode, cidrBits: IntNode): Node = {
    println("Encountered Fn::Cidr. Currently not implemented. Returning NoValue")
    NoValue
  }
}





final case class FindInMapFunction( mappings: Map[String,Map[String,Either[String,Map[String,Any]]]],
                                    ) extends IntrinsicFunction
{
  def apply( m: StringNode, k1: StringNode, k2: Option[StringNode] = None) : GenericValueNode =
  {
    k2 match {
      case None =>
        mappings(m.value)(k1.value) match {
          case Left(s)            => StringNode(s)
          case Right(m1)          => StringNode(m1.toString)
      }
      case Some(k) =>
        mappings(m.value)(k1.value) match {
          case Left(s)            => StringNode(s)
          case Right(m1) =>
            m1(k.value) match {
              case f: Float       => FloatNode(f)
              case b: Boolean     => BooleanNode(b)
              case _              => StringNode(m1(k.value).asInstanceOf[String])
        }
      }
    }
  }
}





final case class GetAttFunction ( resources :Map[String,StackSetResource]) extends IntrinsicFunction
{
  def apply(resourceName:StringNode, attributeName:StringNode): Node =
  {
    if (attributeName.value.equals("arn") && resources!=null && resources.get(resourceName.value).isDefined)
      resources(resourceName.value)
    else if ( resources!=null && resources(resourceName.value).attributes.get(attributeName.value).isDefined )
      resources(resourceName.value).attributes(attributeName.value)
    else
      NoValue
  }
}





// Availability zones might depend on the account and not only on the region!
final case class GetAZsFunction() extends IntrinsicFunction
{
  def apply(reg: StringNode): ListNode[StringNode] =
  {
    // TODO: This is an absolutely fake list
    ListNode[StringNode]( Vector(StringNode(reg+"a"), StringNode(reg+"b"), StringNode(reg+"c")) )
  }
}




final case class ImportValueFunction( outputsByExportName: Map[String,Node],
                                      outputsByLogicalId: Map[String,Node] ) extends IntrinsicFunction
{
  def apply(importName: StringNode): Node =
  {
    outputsByLogicalId.getOrElse(importName.value,
      outputsByExportName.getOrElse(importName.value,
        NoValue))
  }
}




final case class JoinFunction() extends IntrinsicFunction
{
  def apply(delimiter : StringNode, segments : ListNode[StringNode]): StringNode =
  {
    StringNode ( (segments.value map (i => i.value )).mkString(delimiter.value) )
  }
}




final case class SelectFunction() extends IntrinsicFunction
{
  def apply(index:IntNode, list:ListNode[Node]): Node = {
    if (index.value >= list.value.size)
      NoValue
    else
      list.value(index.value)
  }
}




final case class SplitFunction() extends IntrinsicFunction
{
  def apply(delimiter:StringNode, s:StringNode): ListNode[StringNode] =
  {
    ListNode[StringNode]( (s.value.split(delimiter.value) map (s => StringNode(s))).toVector )
  }
}




final case class SubFunction ( resources  : Map[String,StackSetResource],
                               parameters : Map[String,Node] ) extends IntrinsicFunction
{
  def apply(str : StringNode, substitutionMap : Option[Map[String,String]] = None ):StringNode =
  {
    var s = str.value.toLowerCase

    // Round 1: Replace map variables
    if (substitutionMap.isDefined)
    {
      s = substitutionMap.get.foldLeft (s) ((a, b) => a.replaceAll("\\$\\{" + b._1 + "\\}", b._2))
    }

    // Round 2: Replace parameters variables
    s = parameters.foldLeft(s)((a, b) => {
      b._2 match {
        case StringNode(v) =>
          a.replaceAll("\\$\\{" + b._1 + "\\}", v)
        case _ => a
      }
    })

    // Round 3: Replace attributes value
    if (s.contains("."))
      GetAttFunction(resources)(StringNode(s.split("\\.")(0)), StringNode(s.split("\\.")(1)))

    // Round 4: If there are still variables, get rid of special chars and hope they are resource names!
    StringNode ( s.replaceAll("\\$|\\{|\\}", "") )
  }
}





final case class TransformFunction() extends IntrinsicFunction
{
  def apply() : Node =
  {
    NoValue // TODO
  }
}





final case class RefFunction( resources   :Map[String,StackSetResource],
                              parameters  :Map[String,Node],
                              ssE         :Json2StackSetEncoder) extends IntrinsicFunction
{
  def apply(n: Node): Node =
  {
    n match {

      case str: StringNode =>
        if (str.value.startsWith("arn:"))
        {
          val referredNode = ArnFunction(resources,parameters, ssE.resourceByArn)(str.value)
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

      case node: StackSetResource   => node
      case node: ForeignResource    => node
      case _                        => NoValue
    }
  }
}






sealed trait ConditionFunction extends IntrinsicFunction

  final case class IfFunction() extends ConditionFunction {
    def apply(c: BooleanNode, e1: Node, e2: Node): Node =
      if (c.value) e1 else e2
  }


sealed trait BooleanFunction extends ConditionFunction

  final case class AndFunction() extends BooleanFunction {
    def apply(e1: BooleanNode, e2: BooleanNode) : BooleanNode =
      BooleanNode ( e1.value && e2.value )
  }

  final case class OrFunction() extends BooleanFunction {
    def apply(e1: BooleanNode, e2: BooleanNode): BooleanNode =
      BooleanNode ( e1.value || e2.value )
  }

  final case class NotFunction() extends BooleanFunction {
    def apply(e: BooleanNode): BooleanNode =
      BooleanNode ( !e.value )
  }

  final case class EqualsFunction() extends BooleanFunction {
    def apply(e1: Node, e2: Node):BooleanNode =
      BooleanNode( e1 == e2 )
  }
