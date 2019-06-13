package aws.cfn.templates.formalization

import java.nio.charset.StandardCharsets
import java.util.Base64

import aws.cfn.templates.encoding.{Arn, Json2InfrastructureEncoder, Json2ResourceEncoder, Json2StackSetEncoder}

import scala.language.postfixOps



sealed trait IntrinsicFunction





final case class ArnFunction(iE: Json2InfrastructureEncoder, rE:Json2ResourceEncoder,
                             resources   :Map[String,Resource],
                             parameters  :Map[String,Node], arnsMap :Map[String,Node] ) extends IntrinsicFunction
{
  def apply(arnString: String) : Entity = {
    val evaluatedArnStringNode = SubFunction(iE, rE, resources, parameters)(StringNode(arnString)).asInstanceOf[StringNode]
    val arnMatchingResources = new Arn(iE, evaluatedArnStringNode.value).resourcesFromArn()
    arnMatchingResources match {
      case v if v.isEmpty => NoValue
      case v if v.size == 1 => {
        if (rE != null && rE.pointedResourceIsPolicy(v.head))
          iE.updateResByPolicyMap(v.head.asInstanceOf[Resource], rE.resource)
        v.head
      }
      case v => ListOfEntities(v)
    }
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





final case class GetAttFunction ( iE:Json2InfrastructureEncoder, rE: Json2ResourceEncoder,
                                  resources :Map[String,Resource]) extends IntrinsicFunction
{
  def apply(resourceName:StringNode, attributeName:StringNode): Node =
  {
    if (attributeName.value.equals("arn") && resources!=null && resources.get(resourceName.value).isDefined) {
      val res = resources(resourceName.value)
      if ( rE!=null && rE.pointedResourceIsPolicy(res) )
        iE.updateResByPolicyMap(res.asInstanceOf[Resource],
          rE.resource)
      res
    }
    else if ( resources!=null && resources.get(resourceName.value).isDefined
      && resources(resourceName.value).attributes.get(attributeName.value).isDefined )
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




final case class ImportValueFunction( iE: Json2InfrastructureEncoder, rE:Json2ResourceEncoder,
                                      outputsByExportName: Map[String,Node],
                                      outputsByLogicalId: Map[String,Node] ) extends IntrinsicFunction
{
  def apply(importName: StringNode): Node =
  {
    def lookupOtherStackSets() = {
      (iE.stackSetEncoders find (ssE => ssE.outputsByExportName.get(importName.value).isDefined))
        .map (ssE => ssE.outputsByExportName(importName.value)).getOrElse(NoValue)
    }


    val res = outputsByLogicalId.getOrElse(importName.value,
      outputsByExportName.getOrElse(importName.value,
        lookupOtherStackSets()))
    if ( rE!=null && rE.pointedResourceIsPolicy(res) ) iE.updateResByPolicyMap(res.asInstanceOf[Resource],rE.resource)
    res
  }
}




final case class JoinFunction() extends IntrinsicFunction
{
  def apply(delimiter : StringNode, segments : ListNode[StringNode]): Node =
  {
    if (segments.value.asInstanceOf[Vector[Node]] exists ( i => i.isInstanceOf[Resource]) )
      (segments.value.asInstanceOf[Vector[Node]] find ( i => i.isInstanceOf[Resource]))
        .get.asInstanceOf[Resource]
    else
      StringNode ( (segments.value map (i => i.value )).mkString(delimiter.value) )

  }
}




final case class SelectFunction(iE: Json2InfrastructureEncoder, rE:Json2ResourceEncoder) extends IntrinsicFunction
{
  def apply(index:IntNode, list:ListNode[Node]): Node = {
    if (index.value >= list.value.size)
      NoValue
    else {
      val res = list.value(index.value)
      if ( rE!=null && rE.pointedResourceIsPolicy(res) ) iE.updateResByPolicyMap(res.asInstanceOf[Resource],rE.resource)
      res
    }
  }
}




final case class SplitFunction() extends IntrinsicFunction
{
  def apply(delimiter:StringNode, s:StringNode): ListNode[StringNode] =
  {
    ListNode[StringNode]( (s.value.split(delimiter.value) map (s => StringNode(s))).toVector )
  }
}




final case class SubFunction (iE:Json2InfrastructureEncoder, rE:Json2ResourceEncoder,
                              resources  : Map[String,Resource],
                              parameters : Map[String,Node] ) extends IntrinsicFunction
{
  def apply(str : StringNode, substitutionMap : Option[Map[String,String]] = None ):Node =
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
    if (s.contains(".arn}")) {
      s = s.split(".arn\\}").head.split("\\{").last
      if (resources!=null && resources.get(s).isDefined) {
        val res = resources(s)
        if ( rE !=null && rE.pointedResourceIsPolicy(res) ) iE.updateResByPolicyMap(res.asInstanceOf[Resource],rE.resource)
        res
      }
      else NoValue
    }
    else    // Round 4: If there are still variables, get rid of special chars and hope they are resource names!
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





final case class RefFunction(iE: Json2InfrastructureEncoder, rE:Json2ResourceEncoder,
                             resources   :Map[String,Resource],
                             parameters  :Map[String,Node],
                             ssE         :Json2StackSetEncoder) extends IntrinsicFunction
{
  def apply(n: Node) : Node =
  {
    n match {

      case str: StringNode =>
        if (str.value.startsWith("arn:"))
        {
          val res = ArnFunction(iE,rE,resources,parameters, ssE.resourceByArn)(str.value)

          res match {
            case ListOfEntities(v) if v.size==1 => {
              if (!ssE.resourceByArn.values.toVector.contains(v.head))
                ssE.foreignResourcesByArn = ssE.foreignResourcesByArn ++ Map(str.value -> v.head.asInstanceOf[ExternalEntity])
              if ( rE!=null &&  rE.pointedResourceIsPolicy(v.head) )
                iE.updateResByPolicyMap(v.head.asInstanceOf[Resource],rE.resource)
              v.head
            }
            case l:ListOfEntities => {
              l
            }
            case _ =>
              if (!ssE.resourceByArn.values.toVector.contains(res))
                ssE.foreignResourcesByArn = ssE.foreignResourcesByArn ++ Map(str.value -> res.asInstanceOf[ExternalEntity])
              if ( rE!=null &&  rE.pointedResourceIsPolicy(res) )
                iE.updateResByPolicyMap(res.asInstanceOf[Resource],rE.resource)
              res
          }

        }
        else if (parameters!=null && parameters.get(str.value).isDefined){
          parameters(str.value)
        }
        else if (resources!=null && resources.get(str.value).isDefined) {
          val res = resources(str.value)
          if ( rE !=null && rE.pointedResourceIsPolicy(res) ) iE.updateResByPolicyMap(res.asInstanceOf[Resource],rE.resource)
          res
        }
        else NoValue

      case node: Resource   => node
      case node: ExternalEntity    => node
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

