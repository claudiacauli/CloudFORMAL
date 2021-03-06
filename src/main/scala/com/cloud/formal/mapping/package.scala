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

package com.cloud.formal

import argonaut.Json

import scala.language.postfixOps

package object mapping{



  private[mapping]
  object CFnType
  {
    val String  = "string"
    val Float   = "float"
    val Double  = "double"
    val Long    = "long"
    val Int     = "integer"
    val Bool    = "boolean"
    val Timestamp = "timestamp"
    val Cdl       = "commadelimitedlist"
    val Json     = "json"
    val List     = "list"
    val Map      = "map"
    val UnknownType = "unknowntype"
    val UnknownService  = "unknownservicetype"
    val UnknownResource = "unknownresourcetype"
  }


  private[mapping]
  object Specification extends Enumeration
  {
    val ResourceType       	  = "ResourceType"
    val PropertyTypes         = "PropertyTypes"
    val PrimitiveItemType 	  = "PrimitiveItemType"
    val PrimitiveTypeTag      = "PrimitiveType"
    val ItemType           	  = "ItemType"
    val Type               	  = "Type"
    val Properties            = "Properties"
    val Attributes            = "Attributes"
    val Required              = "Required"
    val TypeDelimiter         = "::"
    val SubpropertyDelimiter  = "."
    val SubpropertyDelimiterRegex = "\\."
    val CustomResourceRegex   = "(AWS::CloudFormation::CustomResource|^Custom::.*)"
    val CustomServiceType     = "CloudFormation"
    val CustomResourceType    = "CustomResource"
    val ArnHead               = "arn:"
    val ArnDelimiter          = ":"
    val SpecificationTrailer  = "Specification"
  }


  private[mapping]
  object Renaming
  {
    val AttributePrefix       = "attribute_"
    val Delimiter             = "_"
    val ResSpecName: String => String =
      resSpecFileName =>
        resSpecFileName.split(Specification.SpecificationTrailer).head.toLowerCase
    val ResTypeName: String => String =
      resFullType =>
        resFullType.split(Specification.TypeDelimiter).last.toLowerCase
    val SubpTypeName: String => String =
      subpropType =>
        (if (subpropType.contains(Specification.SubpropertyDelimiter))
          subpropType.split(Specification.SubpropertyDelimiterRegex).last
        else subpropType)
          .toLowerCase
    val PropName: (String,String) => String =
      (domainType,propName) =>
        (domainType+Delimiter+propName).toLowerCase
    val AttrName: (String,String) => String =
      (domainType,attrName) =>
        AttributePrefix+PropName(domainType,attrName)
    val UniquePropName: (String,String,String) => String =
      (resType,domainType,propName) =>
        (resType+Delimiter+domainType+Delimiter+propName).toLowerCase
  }


  private[mapping]
  object JsonUtils
  {

    def getNodesAsMapOfJsons(j:Json): Map[String, Json] =
      subFieldNames(j) zip subFieldContents(j) toMap

    def getNodesAsMapOfStrings(j:Json): Map[String, String] =
      subFieldNames(j) zip subFieldValueContents(j) toMap

    def isList(j: Json): Boolean =
      j.hasField(Specification.Type) &&
        getLCStringField(j,Specification.Type).equals(CFnType.List)

    def ofPrimitive(j: Json): Boolean =
      j.hasField(Specification.PrimitiveItemType)

    def isMap(j: Json): Boolean =
      j.hasField(Specification.Type) &&
        getLCStringField(j,Specification.Type).equals(CFnType.Map)

    def isComplexType(j: Json): Boolean =
      j.hasField(Specification.Type)

    def getPrimitiveType(j : Json): Option[String] =
      if (j.hasField(Specification.PrimitiveTypeTag))
        Some(getLCStringField(j,Specification.PrimitiveTypeTag))
      else
        None

    def subFieldNames(j: Json): List[String] =
      j.objectFields match
      {
        case None     => List()
        case Some(l)  => l map (_.toString.toLowerCase)
      }

    def subFieldContents(j: Json): List[Json] =
      j.objectFieldsOrEmpty map (f => j.fieldOrEmptyArray(f) )

    def subFieldValueContents(j: Json): List[String] =
      j.objectFieldsOrEmpty map (f => getLCStringField(j, f) )

    def getLCStringField(j: Json, field:String): String =
      j.field(field).get.string.get.toLowerCase

  }


}
