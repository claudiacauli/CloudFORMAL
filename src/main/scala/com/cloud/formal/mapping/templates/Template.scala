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

private class Template(val name: String) {

  private[templates] var parameters:   Map[String,GenericValueNode] = _
  private[templates] var mappings:     Map[String,Map[String,Either[String,Map[String,Any]]]] = _
  private[templates] var conditions:   Map[String,Boolean] = _
  private[templates] var outputByLogicalId:  Map[String,Node] = _
  private[templates] var outputByExportName: Map[String,Node] = _
  private[templates] var resources:          Map[String,StackSetResource]  = _
  private[templates] var policyStatements:   Set[Statement] = _

  override def toString: String = {
    "\t\tTemplate: " + name + "\n" +
      prettyString(parameters,"Parameters") +
      prettyString(mappings, "Mappings") +
      prettyString(conditions, "Conditions") +
      prettyString(outputByLogicalId, "Outputs By ID") +
      prettyString(outputByExportName, "Outputs By Export Name")
  }

  private def prettyString(map : Map[String,Any], s : String) = {
    if (map.nonEmpty)
      "\t\t "+s+": \n"   + map.foldLeft("")((a,b)=> a + "\t\t\t("+b._1+" -> "+b._2+")\n")
    else ""
  }

}
