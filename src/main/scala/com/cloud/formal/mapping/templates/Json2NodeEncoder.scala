package com.cloud.formal.mapping.templates

import java.util.regex.Pattern

import argonaut._
import com.cloud.formal.Ontology
import com.cloud.formal.mapping.{CFnType, JsonUtils, Renaming, Specification}
import com.typesafe.scalalogging.LazyLogging

import scala.language.postfixOps

private class Json2NodeEncoder
(val optRE: Option[Json2ResourceEncoder], val tE: Json2TemplateEncoder)
extends LazyLogging
{

  def this(tE : Json2TemplateEncoder){
    this(None,tE)
  }

  def this(rE : Json2ResourceEncoder){
    this(Some(rE), rE.tE)
  }


  private[templates] val ssE: Json2StackSetEncoder = tE.ssE
  private[templates] val iE: Json2InfrastructureEncoder = ssE.iE


  def encode(json: Json, expectedType: Option[(String,String)] = None): Node =
  {
    json match {
      case j if isAwsManagedPolicyArn(j)
      => getAwsManagedPolicyExternalResource(j)
      case j if isArn(j)
      => resolvedArn(j,expectedType)
      case j if j.isNull || isNoValue(j)
      => NoValue
      case j if j.isObject && isIntrinsicFunction(j)
      => evalIntrinsicFunction(j,expectedType)
      case j if j.isObject && isMapProperty(j,expectedType)
      => encodeMapProperty(j,mapProperty(j,expectedType).get)
      case j if j.isObject && isPolicyDoc(j)
      => encodePolicyDoc(j)
      case j if j.isObject && expectedType.isDefined &&
        expectedType.get._2.equals(CFnType.String)
      => StringNode(j.toString())
      case j if j.isObject && j.hasField("Condition")
      => tE.conditions.get(j.field("Condition").get.string.get) match {
        case None     => BooleanNode(false)
        case Some(v)  => BooleanNode(v)
      }
      case j if j.isObject
      => encodeSubproperty(j,expectedType)
      case j if j.isArray
      => encodeArrayNode(j,expectedType)
      case j
      => encodeValueNode(j,expectedType)
    }

  }


  private def resolvedArn(j: Json, spT:Option[(String,String)]) =
    spT match {
      case Some((_,CFnType.String)) => StringNode(j.string.get)
      case _ =>
        val vecNodes = ArnFunction(optRE,tE)(j.string.get)
        vecNodes match {
          case ListOfResources(v) if v.size == 1 => v.head
          case ln:ListOfResources => ln
          case x => x
        }
    }


  private def encodePolicyDoc(j: Json) =
    optRE match {
      case None => logger.warn(s"Json contains a policy in an encoder " +
        s"not associated with a parent resource. Returning NoValue")
        NoValue
      case Some(rE) =>
        val policyEncoder = new Json2PolicyDocumentEncoder(this,j,rE.resource)
        tE.policyEncoders ++= Vector(policyEncoder)
        policyEncoder.policyDocument
    }


  private def encodeValueNode(j: Json, spT: Option[(String,String)])=
    spT match {
      case Some((_, spt))
      => matchNodeBySubpropType(j, spt)
      case None
      => matchNodeByJsonType(j)
    }



  private def encodeArrayNode(j: Json, spT:Option[(String,String)]) =
    ListNode(
      j.array.get
        .map (ji => encode(ji,spT))
        .toVector
    )


  private def encodeMapProperty(j: Json, spT: String) = {
    logger.debug("Encoding of Map Values currently " +
      "not implemented. Returning NoValue")
    NoValue
  }


  private def encodeSubproperty(j: Json, spT:Option[(String,String)]) =
  {
    require(optRE.isDefined)
    val rE = optRE.get

    def givenProperties =
      j.objectFieldsOrEmpty
        .map(f =>
          spT.get._2 + Renaming.Delimiter + f.toString)
        .toSet


    def nodeObjectForProperty(j: Json, propFullName: String)  = {
      val propTemplateName = propFullName
        .split(spT.get._2 + Renaming.Delimiter).last

      val propRange = Json2ResourceEncoder
        .rangeNameOf(
          propFullName,
          rE.resourceOntology,
          rE.serviceType,
          rE.resourceType,ssE)

      encode(
        j.field(propTemplateName).get,
        propRange)
    }


    spT match
    {
      case None  =>

        Subproperty(givenProperties
            .flatMap(propName =>
              Map(propName -> nodeObjectForProperty(j,propName))
            ).toMap)

      case Some((_,spt)) =>

        val absentProperties  =
          Json2ResourceEncoder
            .subPropertiesNamesOfClassName(
              spt, ssE,rE.serviceType,rE.resourceType
              ,rE.resourceOntology) --
            givenProperties
              .map(_.toLowerCase())

        val defaultPropertiesMap =
          absentProperties.flatMap( absProp =>
          DefaultsMap.lookUp(rE.serviceType,absProp) match {
            case None     => Map()
            case Some(v)  => Map(absProp -> rE.assignedDefaultNode(v))
          }).toMap

        val presentPropertiesMap =
          givenProperties
            .flatMap(propName =>
              Map(propName -> nodeObjectForProperty(j,propName)))
            .toMap ++
          defaultPropertiesMap

        Subproperty(
          presentPropertiesMap,
          absentProperties -- defaultPropertiesMap.keys)
    }

  } ensuring optRE.isDefined



  private def evalIntrinsicFunction(j:Json, expectedType:Option[(String,String)]) = {

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


  private def arrayAt(j: Json, fieldName:String, index:Int) =
    j.field(fieldName).get.array match {
      case None
      => j.field(fieldName).get
      case Some(a) if index < a.size
      => a(index)
    }


  private def validateParamsAndEvalBase64(j: Json) =
    encode(j.field(CFnFunTag.Base64UC).get) match {
      case sn: StringNode => Base64Function()(sn)
      case _ =>
        logger.debug(s"Value of Fn::Base64 node $j does not evaluate " +
          "to a StringNode. Returning NoValue")
        NoValue
    }



  private def validateParamsAndEvalCidr(j: Json) =
    CidrFunction()(
      encode(arrayAt(j,CFnFunTag.CidrUC,0)).asInstanceOf[StringNode],
      encode(arrayAt(j,CFnFunTag.CidrUC,1)).asInstanceOf[IntNode],
      encode(arrayAt(j,CFnFunTag.CidrUC,2)).asInstanceOf[IntNode] )


  private def validateParamsAndEvalIf(j: Json, expectedType:Option[(String,String)]) = {

    val encodedCondition  = encode(arrayAt(j, CFnFunTag.IfUC,0))
    def encodedTrueExp() : Node =
      encode(arrayAt(j, CFnFunTag.IfUC, 1),expectedType)
    def encodedFalseExp() : Node =
      encode(arrayAt(j, CFnFunTag.IfUC, 2),expectedType)


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

    val encodedJson = encode(j.field(CFnFunTag.NotUC).get)

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

    val lhs = encode(arrayAt(j, CFnFunTag.AndUC,0))
    val rhs = encode(arrayAt(j,CFnFunTag.AndUC,1))

    (lhs, rhs) match {
      case (l:BooleanNode,r:BooleanNode) => AndFunction()(l,r)
      case _ =>
        logger.debug(s"Either lhs or rhs of Fn::And node $j does not evaluate " +
          "to a BooleanNode. Returning NoValue")
        NoValue
    }
  }


  private def validateParamsAndEvalOr(j: Json) = {

    val lhs  = encode(arrayAt(j, CFnFunTag.OrUC,0))
    val rhs = encode(arrayAt(j, CFnFunTag.OrUC,1))

    (lhs, rhs) match {
      case (l:BooleanNode, r:BooleanNode) => OrFunction()(l,r)
      case _ =>
        logger.debug(s"Either lhs or rhs of Fn::Or node $j does not evaluate " +
          "to a BooleanNode. Returning NoValue")
        NoValue
    }
  }


  private def validateParamsAndEvalEquals(j: Json) = {
    val lhs = encode(arrayAt(j,CFnFunTag.EqualsUC,0))
    val rhs = encode(arrayAt(j,CFnFunTag.EqualsUC,1))

    EqualsFunction()(lhs,rhs)
  }


  private def validateParamsAndEvalFindInMap(j: Json) = {

    val hasSecondLevelKey   = j.field(CFnFunTag.FindInMapUC).get.array.get.size == 3
    val encodedMapName      = encode(arrayAt(j,CFnFunTag.FindInMapUC,0))
    val encodedTopLevelKey  = encode(arrayAt(j,CFnFunTag.FindInMapUC,1))
    val encodedSecondLevelKey = if (hasSecondLevelKey) encode(arrayAt(j,CFnFunTag.FindInMapUC,2)) else None

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

    val encodedResourceName   = encode(arrayAt(j,CFnFunTag.GetAttUC,0))
    val encodedAttributeName  = encode(arrayAt(j,CFnFunTag.GetAttUC,1))

    (encodedResourceName, encodedAttributeName) match {
      case (r:StringNode,a:StringNode)  => GetAttFunction(optRE,tE)(r,a)
      case _ =>
        logger.debug(s"Values of Fn::GetAtt node $j do not " +
          "evaluate to a StringNode type. Returning NoValue")
        NoValue
    }
  }


  private def validateParamsAndEvalGetAZs(j: Json) = {

    val encodedRegion = encode (j.field (CFnFunTag.GetAZsUC).get)

    encodedRegion match {
      case r:StringNode => GetAZsFunction()(r)
      case _ =>
        logger.debug(s"Value of Fn::GetAZs node $j do not" +
          " evaluate to a StringNode. Returning NoValue")
        NoValue
    }
  }


  private def validateParamsAndEvalImportValue(j: Json) = {

    val encodedImportName = encode (j.field (CFnFunTag.ImportValueUC).get)

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
    val encodedDelimiter  = encode (arrayAt(j,CFnFunTag.JoinUC,0),subpropT)
    val encodedList       = encode (arrayAt(j,CFnFunTag.JoinUC,1),subpropT)

    (encodedDelimiter, encodedList) match {
      case (d:StringNode,l:ListNode[Node]) => JoinFunction()(d,l)
      case _ =>
        logger.debug(s"Values of Fn::Join node $j do not " +
          "evaluate to the expected types. Returning NoValue")
        NoValue
    }

  }


  private def validateParamsAndEvalSelect(j: Json) = {

    val encodedIndex = encode(arrayAt(j,CFnFunTag.SelectUC,0))
    val encodedList  = encode(arrayAt(j,CFnFunTag.SelectUC,1))

    (encodedIndex, encodedList) match {
      case (i:IntNode, l:ListNode[Node] ) => SelectFunction(optRE)(i,l)
      case _ =>
        logger.debug(s"Values of Fn::Select node $j do " +
          "not evaluate to the expected types. Returning NoValue")
        NoValue
    }
  }


  private def validateParamsAndEvalSplit(j: Json) = {

    val encodedDelimiter = encode (arrayAt(j,CFnFunTag.SplitUC,0))
    val encodedString = encode (arrayAt(j,CFnFunTag.SplitUC,1))

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
        Some(getNodesAsMapOfEvalStrings(arrayAt(j,CFnFunTag.SubUC,1)))
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
        val encodedStringToMatch = encode(stringToMatch)
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
    encode(j.field(CFnFunTag.RefUC).get) match {
      case NoValue =>
        logger.debug(s"Evaluation of Fn::Ref node $j " +
          s"produced NoValue. Returning NoValue")
        NoValue
      case x => RefFunction(optRE,tE)(x)
    }




  private def getNodesAsMapOfEvalStrings(j:Json) =
    JsonUtils.subFieldNames(j)
      .zip (subFieldValueEvalContents(j))
      .toMap



  private def subFieldValueEvalContents (j: Json) =
    j.objectFieldsOrEmpty
      .map (f =>
        getLowerCaseEvalStringField(j,f))



  private def getLowerCaseEvalStringField(j: Json, field:String) =
    encode(j.field(field).get) match {
      case NoValue => ""
      case StringNode(s) => s
      case StackSetResource(id,_,_,_,_,_)  => id
      case ExternalResource(name,_)        => name
      case x =>
        logger.debug(s"Field supposed to contain either a String " +
          s"or Resource evaluates to unexpected type: $x"+
          ". Returning empty string.")
      ""
    }



  private def matchNodeByJsonType(j: Json) =
    j match {
      case n if n.isNumber && n.number.get.toInt.isDefined
      => IntNode(n.number.get.toInt.get)
      case n if n.isNumber && n.number.get.toLong.isDefined
      => LongNode(n.number.get.toLong.get)
      case n if n.isNumber && n.number.get.toDouble.isDefined
      => DoubleNode(n.number.get.toDouble.get)
      case n if n.isNumber && n.number.get.toFloat.isDefined
      => FloatNode(n.number.get.toFloat.get)
      case n if n.isBool
      => BooleanNode(n.bool.get)
      case n if n.isString
      => StringNode(n.string.get.toLowerCase())
      case n =>
        logger.debug(s"Could not match node $n with a " +
          s"primitive type. Returning NoValue")
        NoValue
    }




  private def matchNodeBySubpropType (j:Json, subpropType:String) = {
    subpropType match {
      case CFnType.String   if j.isString
      => StringNode(j.string.get.toLowerCase())
      case CFnType.Bool  if j.isBool
      => BooleanNode(j.bool.get)
      case CFnType.Int  if j.isNumber && j.number.get.toInt.isDefined
      => IntNode(j.number.get.toInt.get)
      case CFnType.Long   if j.isNumber //&& j.number.get.toLong.isDefined
      => LongNode(j.number.get.truncateToLong)
      case CFnType.Double if j.isNumber && j.number.get.toDouble.isDefined
      => DoubleNode(j.number.get.toDouble.get)
      case CFnType.Float  if j.isNumber && j.number.get.toFloat.isDefined
      => FloatNode(j.number.get.toFloat.get)
      case CFnType.UnknownResource => j match {
        case n if n.isString || n.isNumber || n.isBool =>
          val eR = ExternalResource(n.string.get, iE.infrastructure)
          iE.externalResources ++= Set(eR)
          eR
      }
      case _ => forceSubpropertyType(j,subpropType)
    }

  }





  private def forceSubpropertyType(j:Json, subpropType:String): GenericValueNode =
    subpropType match {
      case CFnType.String if j.isNumber
      => StringNode(j.number.get.toString)
      case CFnType.String
      => StringNode(j.bool.get.toString)
      case CFnType.Bool if j.isString
      => BooleanNode(j.string.get.toBoolean)
      case CFnType.Bool
      => BooleanNode(j.number.get.truncateToInt>0)
      case CFnType.Int if j.isString
      => IntNode(j.string.get.toInt)
      case CFnType.Int if j.isNumber
      => IntNode(j.number.get.truncateToInt)
      case CFnType.Int
      => IntNode(j.bool.get.toString.toInt)
      case CFnType.Long if j.isString
      => LongNode(j.string.get.toLong)
      case CFnType.Long if j.isNumber
      => LongNode(j.number.get.truncateToLong/*.toLong.get*/)
      case CFnType.Long
      => LongNode(j.bool.get.toString.toLong)
      case CFnType.Double if j.isString
      => DoubleNode(j.string.get.toDouble)
      case CFnType.Double if j.isNumber
      => DoubleNode(j.number.get.toDouble.get)
      case CFnType.Double
      => DoubleNode(j.bool.get.toString.toDouble)
      case CFnType.Float if j.isString
      => FloatNode(j.string.get.toFloat)
      case CFnType.Float if j.isNumber
      => FloatNode(j.number.get.toFloat.get)
      case CFnType.Float
      => FloatNode(j.bool.get.toString.toFloat)
      case _ =>
        logger.debug(s"Attempted fallback solution to force " +
          s"conversion from json type to expected subproperty " +
          s"type $subpropType. Failed. Returning StringNode with json content $j. ")
        StringNode(j.toString)
    }


  private def isNoValue(j: Json) =
  (j.isString && j.string.get.toLowerCase.equals(PseudoParameter.NoValue)) ||
    (j.isArray && j.array.get.size==1 && j.array.get(0).isString &&
      j.array.get(0).string.get.toLowerCase.equals(PseudoParameter.NoValue))



  private def isArn(j: Json) =
    j.isString && j.string.get.startsWith(Specification.ArnHead)


  private def isCompleteArn(j:Json) =
    isArn(j) &&
      j.string.get
        .replace("arn:", "")
        .split(":", -1).size >= 5



  private def isNotCompleteArn(j: Json) =
    isArn(j) && !isCompleteArn(j)


  private def resolveNotCompleteArn(j: Json) = {
    println("Found Arn that does not appear to be complete: " + j)
    StringNode(j.string.get)
  }


  private def isAwsManagedPolicyArn(j:Json) =
    j.isString &&
      AwsManagedPolicies.isManagedPolicy(j.string.get)


  private def getAwsManagedPolicyExternalResource(j: Json) =
    AwsManagedPolicy(j.string.get)



  private def isIntrinsicFunction(j:Json)  =
    Pattern
      .compile(CFnFunTag.FunTagRegex, Pattern.CASE_INSENSITIVE)
      .matcher(JsonUtils.subFieldNames(j)(0))
      .matches()


  private def isPolicyDoc(j: Json) = {
    j.hasField(Policy.StatementTag) && j.field(Policy.StatementTag).get.isArray &&
    j.field(Policy.StatementTag).get.array.get(0).hasField(Policy.EffectTag) &&
    j.field(Policy.StatementTag).get.array.get(0).hasField(Policy.ActionTag)
  }


  private def isMapProperty(j:Json, subpropType:Option[(String,String)]) =
    mapProperty(j, subpropType).isDefined


  private def mapProperty(j: Json, subpropType:Option[(String,String)]) =
    subpropType match {
    case Some((_,s)) if s.startsWith(Ontology.MapEntryPrefix)
    => Some(s.split(Ontology.MapEntryPrefix).last)
    case _  => None
  }


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

