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

package com.cloud.formal.mapping.specifications

private class ResourceSpecification(val name: String,
                            val resource: ResourceValueType,
                            val subproperties: Vector[SubpropertyValueType])


private sealed trait NonPrimitiveValueType {
  val name: String
}

  private case class SubpropertyValueType(name: String)
    extends NonPrimitiveValueType {
    private[specifications] var properties: Vector[GenericPropertyType] = _
  }

  private case class ResourceValueType(name: String)
    extends NonPrimitiveValueType {
    private[specifications] var properties: Vector[GenericPropertyType]   = _
    private[specifications] var attributes: Vector[GenericAttributeType]  = _
  }



private sealed trait GenericAttributeOrPropertyType {
  val name: String
}


  private sealed trait GenericAttributeType
    extends GenericAttributeOrPropertyType{
    val domain: ResourceValueType
  }

    private case class StringAttribute
    (name: String, domain: ResourceValueType)
      extends GenericAttributeType

    private case class IntAttribute
    (name: String, domain: ResourceValueType)
      extends GenericAttributeType

    private case class FloatAttribute
    (name: String, domain: ResourceValueType)
      extends GenericAttributeType

    private case class DoubleAttribute
    (name: String, domain: ResourceValueType)
      extends GenericAttributeType

    private case class LongAttribute
    (name: String, domain: ResourceValueType)
      extends GenericAttributeType

    private case class CommaDelimitedListAttribute
    (name: String, domain: ResourceValueType)
      extends GenericAttributeType

    private case class BooleanAttribute
    (name: String, domain: ResourceValueType)
      extends GenericAttributeType

    private case class TimeStamp
    (name: String, domain: ResourceValueType)
      extends GenericAttributeType

    private case class JsonAttribute
    (name:String, domain: ResourceValueType)
      extends GenericAttributeType

    private case class ListOfPrimitiveAttribute
    (primitiveType: String, name:String, domain: ResourceValueType)
      extends GenericAttributeType



  private sealed trait GenericPropertyType
    extends GenericAttributeOrPropertyType{
    val domain: NonPrimitiveValueType
    val req: Boolean
  }

    private case class StringProperty
    (name: String, domain: NonPrimitiveValueType, req:Boolean)
      extends GenericPropertyType

    private case class IntProperty
    (name: String, domain: NonPrimitiveValueType, req:Boolean)
      extends GenericPropertyType

    private case class FloatProperty
    (name: String, domain: NonPrimitiveValueType, req:Boolean)
      extends GenericPropertyType

    private case class DoubleProperty
    (name:String, domain: NonPrimitiveValueType, req:Boolean)
      extends GenericPropertyType

    private case class LongProperty
    (name: String, domain: NonPrimitiveValueType, req:Boolean)
      extends GenericPropertyType

    private case class CommaDelimitedListProperty
    (name: String, domain: NonPrimitiveValueType, req:Boolean)
      extends GenericPropertyType

    private case class BooleanProperty
    (name: String, domain: NonPrimitiveValueType, req:Boolean)
      extends GenericPropertyType

    private case class TimeStampProperty
    (name: String, domain: NonPrimitiveValueType, req:Boolean)
      extends GenericPropertyType

    private case class JsonProperty
    (name:String, domain: NonPrimitiveValueType, req:Boolean)
      extends GenericPropertyType

    private case class ListOfPrimitiveProperty
    (primitiveType: String, name:String, domain: NonPrimitiveValueType, req:Boolean)
      extends GenericPropertyType

    private case class ListOfNonPrimitiveProperty
    (nonPrimitiveType: String, name:String, domain: NonPrimitiveValueType, req:Boolean)
      extends GenericPropertyType

    private case class MapOfPrimitiveProperty
    (primitiveType: String, name:String, domain:NonPrimitiveValueType, req:Boolean)
      extends GenericPropertyType

    private case class MapOfNonPrimitiveProperty
    (nonPrimitiveType: String, name: String, domain: NonPrimitiveValueType, req:Boolean)
      extends GenericPropertyType

    private case class SubpropertyProperty
    (subpropTypeName: String, name:String, domain: NonPrimitiveValueType, req:Boolean)
      extends GenericPropertyType

    private case class ResourceProperty
    (resSpecName: String, resName: String, name: String, domain: NonPrimitiveValueType, req:Boolean)
      extends GenericPropertyType

    private case class ListOfResourcesProperty
    (resSpecName:String, resName:String, name:String, domain:NonPrimitiveValueType, req:Boolean)
      extends GenericPropertyType
