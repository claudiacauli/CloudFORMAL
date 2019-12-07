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
  private[templates] val ifEval: IntrinsicFunctionEvaluator = new IntrinsicFunctionEvaluator(this,tE,optRE)


  def encode(json: Json, expectedType: Option[(String,String)] = None): Node =
  {
    json match {
      case j if isAwsManagedPolicyArn(j)
      => encodeValueNode(j,None)
      case j if isArn(j)
      => resolvedArn(j,expectedType)
      case j if j.isNull || isNoValue(j)
      => NoValue
      case j if j.isObject && isIntrinsicFunction(j)
      => ifEval.eval(j,expectedType)
      case j if j.isObject && isMapProperty(j,expectedType)
      => encodeMapProperty(j,mapProperty(j,expectedType).get)
      case j if j.isObject && isPolicyDoc(j)
      => encodeValueNode(j,Some(("", CFnType.String)))
      case j if j.isObject && expectedType.isDefined &&
        expectedType.get._2.equals(CFnType.String)
      => StringNode(j.toString())
      case j if j.isObject && j.hasField(TemplateTag.Condition)
      => tE.conditions.get(j.field(TemplateTag.Condition).get.string.get) match {
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
          absentProperties -- defaultPropertiesMap.keySet)
    }

  } ensuring optRE.isDefined



  def getNodesAsMapOfEvalStrings(j:Json) =
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
      case CFnType.String if j.isObject
      => StringNode(j.toString)
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



  private def isAwsManagedPolicyArn(j:Json) =
    j.isString &&
      AwsManagedPoliciesHashSet
        .contains(j.string.get.toLowerCase)




  private def isArn(j: Json) =
    j.isString && j.string.get.startsWith(Specification.ArnHead)



  private def isIntrinsicFunction(j:Json)  =
    if (j.isNull || (j.isObject && j.objectFieldsOrEmpty.isEmpty))
      false
    else Pattern
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




}

