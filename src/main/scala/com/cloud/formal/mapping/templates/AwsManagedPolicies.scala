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

import argonaut.{Json, Parse}
import com.cloud.formal.FilePath
import com.typesafe.scalalogging.LazyLogging

import scala.io.Source


/*
List of All Aws Managed Policies taken from:
https://gist.github.com/gene1wood/55b358748be3c314f956
 */

object AwsManagedPolicies
  extends LazyLogging
{

  private var awsManagedPolicies: Map[String,Json] = _


  private[templates]
  def isManagedPolicy(arn: String) =
    if (awsManagedPolicies == null) {
      logger.error("Attempting to access AwsManagedPolicies " +
        "map without loading it first. Must be loaded by calling " +
        " method loadManagedPolicies().")
      false
    }
    else
      awsManagedPolicies.get(arn).isDefined


  private[templates]
  def getPolicyJson(arn: String) =
    awsManagedPolicies(arn)


  private[templates]
  def loadManagedPolicies(): Unit = {
    awsManagedPolicies =
      Parse.parseOption(Source
        .fromResource(FilePath.AwsManagedPolicies)
        .getLines().mkString) match {
        case None
        => logger.error(s"Failed to parse ${FilePath.AwsManagedPolicies}" +
          s" file. It might be malformed.");
          Map()
        case Some(j)
        => arns(j).zip(jsonPolicyDocs(j)).toMap
      }
  }


  private def arns(j: Json) : List[String] =
    j.objectFieldsOrEmpty
      .map(f =>
        j.field(f).get
          .field("Arn").get.string.get)



  private def jsonPolicyDocs(j: Json)=
    j.objectFieldsOrEmpty
      .map(f =>
        j.field(f).get
          .field("Document").get)

}
