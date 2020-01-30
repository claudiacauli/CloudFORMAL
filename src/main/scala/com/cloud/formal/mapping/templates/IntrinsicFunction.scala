/*
 *    Copyright 2019 Claudia Cauli
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */

package com.cloud.formal.mapping.templates

import java.nio.charset.StandardCharsets
import java.util.Base64

import com.cloud.formal.mapping.Specification
import com.typesafe.scalalogging.{LazyLogging, StrictLogging}

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
            StringNode(resourceName.v + "_attribute_" + attributeName.v)
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
    //println("Need to implement a GetAZ Function")
    // TODO: This is an absolutely fake list
    ListNode[StringNode]( Vector(StringNode(reg.v+"a"), StringNode(reg.v+"b"), StringNode(reg.v+"c")) )
  }
}





private final case class ImportValueFunction( tE: Json2TemplateEncoder,
                                      optRE:Option[Json2ResourceEncoder])
  extends IntrinsicFunction
  with (StringNode => Node)
{
  def apply(importName: StringNode): Node =
  {
    // Assuming that a SS is a collection of templates in the same account, then the importvalue must be looked for in the same account
    def lookupOtherStackSets =
      tE.ssE.iE.stackSetEncoders
        .find(_.outputsByExportName.get(importName.value).isDefined)
        .map(_.outputsByExportName(importName.value))
        .getOrElse(NoValue)

    val res =
      tE.outputByLogicalId
        .getOrElse(importName.value,
          tE.ssE.outputsByExportName
            .getOrElse(importName.value,
              lookupOtherStackSets))
    res
  }
}





private final case class JoinFunction()
  extends IntrinsicFunction
  with ((StringNode, ListNode[Node]) => Node)
  with LazyLogging
{
  def apply(delimiter: StringNode, segments: ListNode[Node]): StringNode =
  {
    StringNode(segments.value.map({
      case StringNode(v)                => v
      case IntNode(i)                   => i.toString
      case FloatNode(f)                 => f.toString
      case LongNode(f)                  => f.toString
      case DoubleNode(d)                => d.toString
      case BooleanNode(b)               => b.toString
      case ExternalResource(n,_)        => n
      case StackSetResource(id,_,_,_,_,_) => id
      case NoValue => ""
      case _ => logger.warn(s"Cannot Evaluate Fn::Joing function. Received unexpected value")
        ""
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
  def apply(referredNode: Node): Node = {
    val ret = referredNode match {
      case ssR: StackSetResource => ssR
      case eR:  ExternalResource => eR
      case StringNode(s) if s.startsWith(Specification.ArnHead) =>
        ArnFunction(optRE,tE)(s) match {
          case ListOfResources(v) if v.size==1 =>
            //println("Here the Ref Function is returning only one resource!")
            updateResourceByArnMap(optRE,v.head,s)
            v.head
          case l:ListOfResources => l
          case r =>
            updateResourceByArnMap(optRE,r,s)
            r
        }
      case StringNode(s) if tE.parameters!=null && tE.parameters.get(s).isDefined
      => val pV = tE.parameters(s)
        pV match {
          case node: StringNode =>
            if (tE.resources!=null)
            tE.resources.find(_._2.resourceName ==
              node.v.toLowerCase) match {
              case None =>
                tE.parameters(s)
              case Some(p) => p._2
          } else tE.parameters(s)
          case _ => tE.parameters(s)
        }
      case StringNode(s) if tE.resources!=null && tE.resources.get(s).isDefined
      => tE.resources(s)
      case _  => NoValue
    }
    ret
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
    def apply(e1: Node, e2: Node):BooleanNode = {
      (e1,e2) match {
        case (bn1:BooleanNode, bn2:BooleanNode) =>  BooleanNode(bn1.value == bn2.value)
        case (bn1:BooleanNode, sn1:StringNode)  if bn1.value => BooleanNode(sn1.value.toLowerCase()=="true")
        case (bn1:BooleanNode, sn1:StringNode)  if !bn1.value => BooleanNode(sn1.value.toLowerCase()=="false")
        case (bn1:BooleanNode, in1:IntNode)     if bn1.value => BooleanNode(in1.value==1)
        case (bn1:BooleanNode, in1:IntNode)     if !bn1.value => BooleanNode(in1.value==0)
        case (bn1:BooleanNode, in1:LongNode)     if bn1.value => BooleanNode(in1.value==1)
        case (bn1:BooleanNode, in1:LongNode)     if !bn1.value => BooleanNode(in1.value==0)
        case (in1:IntNode, sn1:StringNode) => BooleanNode(sn1.value.toInt==in1.value)
        case (in1:IntNode, bn1:BooleanNode)     if bn1.value => BooleanNode(in1.value==1)
        case (in1:IntNode, bn1:BooleanNode)     if !bn1.value => BooleanNode(in1.value==0)
        case (in1:IntNode, bn1:BooleanNode)     if bn1.value => BooleanNode(in1.value==1)
        case (in1:IntNode, bn1:BooleanNode)     if !bn1.value => BooleanNode(in1.value==0)
        case (in1:IntNode, in2:IntNode)   => BooleanNode(in1.value==in2.value)
        case (ln1:LongNode, in1:IntNode)  => BooleanNode(ln1.value == in1.value)
        case (ln1:LongNode, ln2:LongNode) => BooleanNode(ln1.value == ln2.value)
        case (in1:LongNode, bn1:BooleanNode)     if bn1.value => BooleanNode(in1.value==1)
        case (in1:LongNode, bn1:BooleanNode)     if !bn1.value => BooleanNode(in1.value==0)
        case (in1:LongNode, sn1:StringNode) => BooleanNode(sn1.value.toLong==in1.value)
        case (sn1:StringNode, in1:IntNode) => BooleanNode(sn1.value.toInt==in1.value)
        case (sn1:StringNode, bn1:BooleanNode)  if bn1.value => BooleanNode(sn1.value.toLowerCase()=="true")
        case (sn1:StringNode, bn1:BooleanNode)  if !bn1.value => BooleanNode(sn1.value.toLowerCase()=="false")
        case (sn1:StringNode, sn2:StringNode) => BooleanNode(sn1.value.toLowerCase()==sn2.value.toLowerCase())
        case _=>  BooleanNode( e1 == e2 )
      }
    }

  }