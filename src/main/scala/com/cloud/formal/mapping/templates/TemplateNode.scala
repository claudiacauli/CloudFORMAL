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

import argonaut.Json

private[templates]
sealed trait Node


  private[templates]
  sealed trait GenericValueNode
    extends Node


    private[templates]
    case object NoValue
      extends GenericValueNode
        with Entity


    private[templates]
    sealed abstract class ValueNode[T](val value: T)
      extends GenericValueNode
    {
      override def toString: String = value.toString
    }


      private[templates]
      final case class StringNode(v: String)
        extends ValueNode[String](v)

      private[templates]
      final case class BooleanNode(v: Boolean)
        extends ValueNode[Boolean](v)

      private[templates]
      final case class IntNode(v: Int)
        extends ValueNode[Int](v)

      private[templates]
      final case class FloatNode(v: Float)
        extends ValueNode[Float](v)

      private[templates]
      final case class DoubleNode(v: Double)
        extends ValueNode[Double](v)

      private[templates]
      final case class LongNode(v: Long)
        extends ValueNode[Long](v)

      private[templates]
      final case class JsonNode(v: String)
        extends ValueNode[String](v)

      private[templates]
      final case class CommaDelimitedListNode(v: String)
        extends ValueNode[String](v)

      private[templates]
      final case class TimeStampNode(v: String)
        extends ValueNode[String](v)

      private[templates]
      final case class ListNode[T<:Node](value: Vector[T])
        extends GenericValueNode
      {
        override def toString: String = value.toString()
      }

      private[templates]
      final case class MapNode[T<:Node](value: Map[String,T])
        extends GenericValueNode
      {
        override def toString: String = value.toString()
      }




  private[templates]
  sealed trait ObjectNode
    extends Node


    private[templates]
    sealed trait Entity
      extends ObjectNode


      private[templates]
      sealed trait Principal
        extends Entity


      private[templates]
      case object Public
        extends Principal
      {
        override def toString : String = "Public"
      }


      private[templates]
      final case class ServicePrincipal(name:String)
        extends Principal
      {
        override def toString: String = "ServicePrincipal(" + name + ")"
      }


      private[templates]
      final case class AccountPrincipal(accountId:String)
        extends Principal
      {
        override def toString: String = "AccountPrincipal("+accountId+")"
      }


      private[templates]
      final case class FederatedAccountPrincipal(federation:String)
        extends Principal
      {
        override def toString: String = "FederatedAccountPrincipal("+federation+")"
      }


      private[templates]
      final case class CanonicalUserPrincipal(canonicalUserId:String)
        extends Principal
      {
        override def toString: String = "CanonicalUserPrincipal("+canonicalUserId+")"
      }


      private[templates]
      sealed trait Resource
        extends Principal


      private[templates]
      final case class ExternalResource(name:String, infrastructure:Infrastructure=null)
        extends Resource
      {
        override def toString: String = "ExternalEntity(" + name + ")"
      }


      private[templates]
      final case class StackSetResource
      (resourceLogicalId : String, serviceType : String,
       resourceType : String, stackset: StackSet, template: Template,
       attributes : Map[String, GenericValueNode])
        extends Resource
      {
        val value: String = resourceLogicalId
        var resourceName : String = resourceLogicalId
        var givenProperties: Map[String,Node] = Map()
        var absentProperties: Set[String] = Set()
        def apply(): StackSetResource = this

        override def toString: String = {
         serviceType + "::" + resourceType + "(" + resourceLogicalId + ")"
        }

      }


    private[templates]
    final case class ListOfResources(nodes:Vector[Resource])
      extends Resource


    private[templates]
    final case class Subproperty
    (givenProperties  : Map[String,Node],
     absentProperties : Set[String] = Set())
      extends ObjectNode
    {
      def apply(): Subproperty = this
    }


    private[templates]
    final case class PolicyDocument(statements: Set[Statement])
      extends ObjectNode
    {
      def apply(): PolicyDocument = this
    }


    private[templates]
    final case class AwsManagedPolicy(name: String)
      extends ObjectNode


    private[templates]
    final case class StatementJsonWrapper(json: Json)
      extends ObjectNode


    private[templates]
    sealed abstract class Statement ( val principals: (Boolean,Set[Principal]),
                                      val actions: (Boolean,Vector[String]),
                                      val resources: (Boolean, Vector[Resource]),
                                      val hasCondition: Boolean,
                                      val isAssumeRoleStatement: Boolean,
                                      val account: String)
      //extends ObjectNode
    {

      def list(l: Vector[Any]): String = {
        l.foldLeft("")((a,b)=>a+b+" ")
      }

      def pretty(a: (Boolean,Vector[Any])) : String = {
        if (!a._1) " NOT { " + list(a._2) + " }" else " { " + list(a._2) + " }"
      }

      def listS(l: Set[Principal]): String = {
        l.foldLeft("")((a,b)=>a+b+" ")
      }

      def prettyS(a: (Boolean,Set[Principal])) : String = {
        if (!a._1) " NOT { " + listS(a._2) + " }" else " { " + listS(a._2) + " }"
      }

      def prettyB(hasC: Boolean): String = {
        if (hasC) "\n under a condition."
        else "\n with NO condition."
      }

    }

      private[templates]
      final case class AllowStatement(p: (Boolean,Set[Principal]),
                                      a: (Boolean,Vector[String]),
                                      r: (Boolean, Vector[Resource]),
                                      hasC: Boolean ,
                                      isARS: Boolean,
                                      acc: String)
        extends Statement(p,a,r,hasC,isARS,acc)
      {
        override def toString: String = {
          "Allows \n principals " + prettyS(p) +
            "\n to perform actions " + pretty(a) + "\n on resources " + pretty(r) + prettyB(hasC)
        }
      }



      private[templates]
      final case class DenyStatement(p: (Boolean,Set[Principal]),
                                     a: (Boolean,Vector[String]),
                                     r: (Boolean, Vector[Resource]),
                                     hasC: Boolean ,
                                     isARS: Boolean,
                                     acc: String)
        extends Statement(p,a,r,hasC,isARS,acc)
      {
        override def toString: String = {
          "Denies \n principals" + prettyS(p) +
            "\n to perform actions " + pretty(a) + "\n on resources " + pretty(r) + prettyB(hasC)
        }
      }
