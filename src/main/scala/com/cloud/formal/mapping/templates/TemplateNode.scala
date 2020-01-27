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
      sealed trait Resource
        extends Entity

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
        var resourceName : String = _
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
