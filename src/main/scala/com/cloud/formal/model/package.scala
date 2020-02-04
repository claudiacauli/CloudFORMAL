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

package object model{


  private[model]
  object Format extends  Enumeration
  {
    val RdfXml         = "rdf"
    val OwlXml         = "xml"
    val Turtle      = "ttl"
    val Functional  = "fun"
    val DefaultFormat: String = OwlXml
  }


  private[model]
  object ModelSuffix extends Enumeration
  {
    val StackSet        = "_StackSetModel"
    val Infrastructure  = "_InfrastructureModel"
  }


  private[formal]
  object ModelFileSuffix extends Enumeration
  {
    val StackSet: String        = ModelSuffix.StackSet + Extension.Owl
    val Infrastructure: String  = ModelSuffix.Infrastructure + Extension.Owl
  }


  private[model]
  object ProtegeCatalogue
  {
    val FileName: String = "catalog-v001.xml"
  }


}

