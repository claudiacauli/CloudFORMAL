package com.cloud.formal.mapping.templates

import argonaut.Json
import com.cloud.formal.mapping.{CFnType, JsonUtils, Specification}
import com.typesafe.scalalogging.LazyLogging


private class IntrinsicFunctionEvaluator
(val nE: Json2NodeEncoder, val tE: Json2TemplateEncoder, val optRE: Option[Json2ResourceEncoder])
extends LazyLogging
{


  def eval(j:Json, expectedType:Option[(String,String)]) = {

    JsonUtils.subFieldNames(j)(0) match
    {
      case CFnFunTag.Base64
      => matchWithExpectedTypeIfAny(
        validateParamsAndEvalBase64(j),expectedType)
      case CFnFunTag.Cidr
      => matchWithExpectedTypeIfAny(
        validateParamsAndEvalCidr(j),expectedType)
      case CFnFunTag.If
      => matchWithExpectedTypeIfAny(
        validateParamsAndEvalIf(j,expectedType),expectedType)
      case CFnFunTag.Not
      => matchWithExpectedTypeIfAny(
        validateParamsAndEvalNot(j),expectedType)
      case CFnFunTag.And
      => matchWithExpectedTypeIfAny(
        validateParamsAndEvalAnd(j),expectedType)
      case CFnFunTag.Equals
      => matchWithExpectedTypeIfAny(
        validateParamsAndEvalEquals(j),expectedType)
      case CFnFunTag.Or
      => matchWithExpectedTypeIfAny(
        validateParamsAndEvalOr(j),expectedType)
      case CFnFunTag.FindInMap
      => matchWithExpectedTypeIfAny(
        validateParamsAndEvalFindInMap(j),expectedType)
      case CFnFunTag.GetAtt
      => matchWithExpectedTypeIfAny(
        validateParamsAndEvalGetAtt(j),expectedType)
      case CFnFunTag.GetAZs
      => matchWithExpectedTypeIfAny(
        validateParamsAndEvalGetAZs(j),expectedType)
      case CFnFunTag.ImportValue
      => matchWithExpectedTypeIfAny(
        validateParamsAndEvalImportValue(j),expectedType)
      case CFnFunTag.Join
      => matchWithExpectedTypeIfAny(
        validateParamsAndEvalJoin(j,Some("",CFnType.String)),Some("",CFnType.String))
      case CFnFunTag.Select
      => matchWithExpectedTypeIfAny(
        validateParamsAndEvalSelect(j),expectedType)
      case CFnFunTag.Split
      => matchWithExpectedTypeIfAny(
        validateParamsAndEvalSplit(j),Some("",CFnType.String))
      case CFnFunTag.Sub
      => matchWithExpectedTypeIfAny(
        validateParamsAndEvalSub(j),expectedType)
      case CFnFunTag.Transform
      => matchWithExpectedTypeIfAny(
        validateParamsAndEvalTransform(j),expectedType)
      case CFnFunTag.Ref
      => matchWithExpectedTypeIfAny(
        validateParamsAndEvalRef(j),expectedType)
      case _ =>
        logger.warn(s"Json object $j is intrinsic function but does not " +
          "contain any of the CFn functions tags. Returning NoValue")
        NoValue
    }

  }





  private def validateParamsAndEvalBase64(j: Json) =
    nE.encode(j.field(CFnFunTag.Base64UC).get) match {
      case sn: StringNode => Base64Function()(sn)
      case _ =>
        logger.debug(s"Value of Fn::Base64 node $j does not evaluate " +
          "to a StringNode. Returning NoValue")
        NoValue
    }



  private def validateParamsAndEvalCidr(j: Json) =
    CidrFunction()(
      nE.encode(arrayAt(j,CFnFunTag.CidrUC,0)).asInstanceOf[StringNode],
      nE.encode(arrayAt(j,CFnFunTag.CidrUC,1)).asInstanceOf[IntNode],
      nE.encode(arrayAt(j,CFnFunTag.CidrUC,2)).asInstanceOf[IntNode] )


  private def validateParamsAndEvalIf(j: Json, expectedType:Option[(String,String)]) = {

    val encodedCondition  = nE.encode(arrayAt(j, CFnFunTag.IfUC,0))
    def encodedTrueExp() : Node =
      nE.encode(arrayAt(j, CFnFunTag.IfUC, 1),expectedType)
    def encodedFalseExp() : Node =
      nE.encode(arrayAt(j, CFnFunTag.IfUC, 2),expectedType)


    encodedCondition match {
      case sn: StringNode   => tE.conditions.get(sn.value) match {
        case None     => NoValue
        case Some(b) if b =>
          IfFunction()(BooleanNode(b), encodedTrueExp(), NoValue)
        case Some(b) if !b =>
          IfFunction()(BooleanNode(b), NoValue, encodedFalseExp())
      }
      case bn : BooleanNode if bn.value =>
        IfFunction()(bn, encodedTrueExp(), NoValue)
      case bn : BooleanNode if !bn.value =>
        IfFunction()(bn, NoValue, encodedFalseExp())
      case _                =>
        logger.debug(s"Value of Fn::If node $j does not evaluate " +
          "to a BooleanNode. Returning NoValue")
        NoValue
    }

  }


  private def validateParamsAndEvalNot(j: Json) = {

    val encodedJson = nE.encode(j.field(CFnFunTag.NotUC).get)

    encodedJson match {
      case bn : BooleanNode                        => NotFunction()(bn)
      case l  : ListNode[BooleanNode]             => NotFunction()(l.value.head)
      case _ =>
        logger.debug(s"Value of Fn::Not node $j does not evaluate " +
          "to a BooleanNode. Returning NoValue")
        NoValue
    }
  }


  private def validateParamsAndEvalAnd(j: Json) = {

    val lhs = nE.encode(arrayAt(j, CFnFunTag.AndUC,0))
    val rhs = nE.encode(arrayAt(j,CFnFunTag.AndUC,1))

    (lhs, rhs) match {
      case (l:BooleanNode,r:BooleanNode) => AndFunction()(l,r)
      case _ =>
        logger.debug(s"Either lhs or rhs of Fn::And node $j does not evaluate " +
          "to a BooleanNode. Returning NoValue")
        NoValue
    }
  }


  private def validateParamsAndEvalOr(j: Json) = {

    val lhs  = nE.encode(arrayAt(j, CFnFunTag.OrUC,0))
    val rhs = nE.encode(arrayAt(j, CFnFunTag.OrUC,1))

    (lhs, rhs) match {
      case (l:BooleanNode, r:BooleanNode) => OrFunction()(l,r)
      case _ =>
        logger.debug(s"Either lhs or rhs of Fn::Or node $j does not evaluate " +
          "to a BooleanNode. Returning NoValue")
        NoValue
    }
  }


  private def validateParamsAndEvalEquals(j: Json) = {
    val lhs = nE.encode(arrayAt(j,CFnFunTag.EqualsUC,0))
    val rhs = nE.encode(arrayAt(j,CFnFunTag.EqualsUC,1))

    EqualsFunction()(lhs,rhs)
  }


  private def validateParamsAndEvalFindInMap(j: Json) = {

    val hasSecondLevelKey   = j.field(CFnFunTag.FindInMapUC).get.array.get.size == 3
    val encodedMapName      = nE.encode(arrayAt(j,CFnFunTag.FindInMapUC,0))
    val encodedTopLevelKey  = nE.encode(arrayAt(j,CFnFunTag.FindInMapUC,1))
    val encodedSecondLevelKey = if (hasSecondLevelKey) nE.encode(arrayAt(j,CFnFunTag.FindInMapUC,2)) else None

    (encodedMapName,encodedTopLevelKey,encodedSecondLevelKey) match {
      case (m:StringNode,k1:StringNode,None)                => FindInMapFunction(tE.mappings)(m,k1)
      case (m:StringNode,k1:StringNode,k2:StringNode)       => FindInMapFunction(tE.mappings)(m,k1,Some(k2))
      case _ =>
        logger.debug(s"Value of Fn::FindInMap node $j does not " +
          "evaluate to the right type or requested key not found " +
          "in templates mappings. Returning NoValue")
        NoValue
    }
  }


  private def validateParamsAndEvalGetAtt(j: Json) = {

    val encodedResourceName   = nE.encode(arrayAt(j,CFnFunTag.GetAttUC,0))
    val encodedAttributeName  = nE.encode(arrayAt(j,CFnFunTag.GetAttUC,1))

    (encodedResourceName, encodedAttributeName) match {
      case (r:StringNode,a:StringNode)  => GetAttFunction(optRE,tE)(r,a)
      case _ =>
        logger.debug(s"Values of Fn::GetAtt node $j do not " +
          "evaluate to a StringNode type. Returning NoValue")
        NoValue
    }
  }


  private def validateParamsAndEvalGetAZs(j: Json) = {

    val encodedRegion = nE.encode(j.field (CFnFunTag.GetAZsUC).get)

    encodedRegion match {
      case r:StringNode => GetAZsFunction()(r)
      case _ =>
        logger.debug(s"Value of Fn::GetAZs node $j do not" +
          " evaluate to a StringNode. Returning NoValue")
        NoValue
    }
  }


  private def validateParamsAndEvalImportValue(j: Json) = {

    val encodedImportName = nE.encode(j.field (CFnFunTag.ImportValueUC).get)

    encodedImportName match {
      case i:StringNode => ImportValueFunction(tE, optRE)(i)
      case _ =>
        logger.debug(s"Value of Fn::ImportValue node $j does" +
          " not evaluate to a StringNode. Returning NoValue")
        NoValue
    }
  }


  private def validateParamsAndEvalJoin(j: Json,  subpropT: Option[(String,String)]) =
  {
    val encodedDelimiter  = nE.encode(arrayAt(j,CFnFunTag.JoinUC,0),subpropT)
    val encodedList       = nE.encode(arrayAt(j,CFnFunTag.JoinUC,1),subpropT)

    (encodedDelimiter, encodedList) match {
      case (d:StringNode,l:ListNode[Node]) =>
        val s = JoinFunction()(d,l)
        if (s.value.startsWith(Specification.ArnHead))
          ArnFunction(optRE,tE)(s.value)
        s
      case _ =>
        logger.debug(s"Values of Fn::Join node $j do not " +
          "evaluate to the expected types. Returning NoValue")
        NoValue
    }

  }


  private def validateParamsAndEvalSelect(j: Json) = {

    val encodedIndex = nE.encode(arrayAt(j,CFnFunTag.SelectUC,0))
    val encodedList  = nE.encode(arrayAt(j,CFnFunTag.SelectUC,1))

    (encodedIndex, encodedList) match {
      case (i:IntNode, l:ListNode[Node] ) => SelectFunction(optRE)(i,l)
      case _ =>
        logger.debug(s"Values of Fn::Select node $j do " +
          "not evaluate to the expected types. Returning NoValue")
        NoValue
    }
  }


  private def validateParamsAndEvalSplit(j: Json) = {

    val encodedDelimiter = nE.encode(arrayAt(j,CFnFunTag.SplitUC,0))
    val encodedString = nE.encode(arrayAt(j,CFnFunTag.SplitUC,1))

    (encodedDelimiter, encodedString) match {
      case (d:StringNode, s:StringNode) => SplitFunction ()(d,s)
      case _ =>
        logger.debug(s"Values of Fn::Split node $j do " +
          "not evaluate to StringNode. Returning NoValue")
        NoValue
    }
  }


  private def validateParamsAndEvalSub(j: Json) = {

    val stringToMatch =
      arrayAt(j,CFnFunTag.SubUC,0)
    val isArray =
      j.field(CFnFunTag.SubUC).get.isArray
    val isArrayWithTwoElements =
      isArray && j.field(CFnFunTag.SubUC).get.array.get.size == 2
    val substitutionMap =
      if (isArrayWithTwoElements)
        Some(nE.getNodesAsMapOfEvalStrings(arrayAt(j,CFnFunTag.SubUC,1)))
      else
        None

    stringToMatch match {
      case n if isCompleteArn(n) =>
        SubFunction(optRE,tE)(StringNode(n.string.get), substitutionMap) match {
          case StringNode(s) => ArnFunction(optRE,tE)(s)
          case _ =>
            logger.debug(s"Evaluation of Fn::Sub node $n, supposed " +
              s"to contain an ARN, did not produce a " +
              s"StringNode. Returning NoValue")
            NoValue
        }
      case _ =>
        val encodedStringToMatch = nE.encode(stringToMatch)
        (substitutionMap,encodedStringToMatch) match {
          case (None,s:StringNode)    => SubFunction(optRE,tE)(s)
          case (_,s:StringNode)       => SubFunction(optRE,tE)(s,substitutionMap)
          case _ =>
            logger.debug(s"Values of Fn::Sub node $j do not " +
              "evaluate to the expected types. Returning NoValue")
            NoValue
        }
    }
  }


  private def validateParamsAndEvalTransform(j: Json) = {
    // TODO
    logger.warn("Intrinsic Function Fn::Transform not " +
      "implemented. Returning NoValue")
    NoValue
  }


  private def validateParamsAndEvalRef(j: Json) =
    nE.encode(j.field(CFnFunTag.RefUC).get) match {
      case NoValue =>
        logger.debug(s"Evaluation of Fn::Ref node $j " +
          s"produced NoValue. Returning NoValue")
        NoValue
      case x => RefFunction(optRE,tE)(x)
    }



  private def arrayAt(j: Json, fieldName:String, index:Int) =
    j.field(fieldName).get.array match {
      case None
      => j.field(fieldName).get
      case Some(a) if index < a.size
      => a(index)
    }



  private def isCompleteArn(j:Json) =
    j.isString && j.string.get.startsWith(Specification.ArnHead) &&
      j.string.get
        .replace("arn:", "")
        .split(":", -1).size >= 5



  private def matchWithExpectedTypeIfAny(evalFunNode: Node,subpropType: Option[(String,String)]) =
    (subpropType,evalFunNode) match {
      case (Some((_,x)),y) =>
        (x,y) match {
          case (CFnType.Int,StringNode(v))   => IntNode(v.toInt)
          case (CFnType.Int,DoubleNode(v))   => IntNode(v.toInt)
          case (CFnType.Int,FloatNode(v))    => IntNode(v.toInt)
          case (CFnType.Int,LongNode(v))     => IntNode(v.toInt)
          case (CFnType.Double,StringNode(v))    => DoubleNode(v.toDouble)
          case (CFnType.Double,IntNode(v))       => DoubleNode(v.toDouble)
          case (CFnType.Double,FloatNode(v))     => DoubleNode(v.toDouble)
          case (CFnType.Double,LongNode(v))      => DoubleNode(v.toDouble)
          case (CFnType.Long,StringNode(v))      => LongNode(v.toLong)
          case (CFnType.Long,IntNode(v))         => LongNode(v.toLong)
          case (CFnType.Long,FloatNode(v))       => LongNode(v.toLong)
          case (CFnType.Long,DoubleNode(v))      => LongNode(v.toLong)
          case (CFnType.Float,StringNode(v))     => FloatNode(v.toFloat)
          case (CFnType.Float,IntNode(v))        => FloatNode(v.toFloat)
          case (CFnType.Float,LongNode(v))       => FloatNode(v.toFloat)
          case (CFnType.Float,DoubleNode(v))     => FloatNode(v.toFloat)
          case (CFnType.String,IntNode(i))       => StringNode(i.toString)
          case (CFnType.String,DoubleNode(d))    => StringNode(d.toString)
          case (CFnType.String,FloatNode(f))     => StringNode(f.toString)
          case (CFnType.String,BooleanNode(b))   => StringNode(b.toString)
          case (CFnType.String,r:StackSetResource) => StringNode(r.resourceName)
          case _ => evalFunNode
        }
      case _ => evalFunNode
    }


}
