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
import com.typesafe.scalalogging.{LazyLogging, StrictLogging}

import scala.collection.mutable

object Json2InfrastructureEncoder {

  def encode(stackSets: Vector[(Vector[(String,Json,Option[Json])],String)], infrastructureName:String): Infrastructure = {
    new Json2InfrastructureEncoder(stackSets,infrastructureName).encode()
  }

}


private class Json2InfrastructureEncoder
(stackSets: Vector[(Vector[(String,Json,Option[Json])],String)], infrastructureName:String)
extends StrictLogging
{

  logger.info(s"Initializing $infrastructureName Infrastructure Encoder and sub-Encoders.")

  private[templates] val infrastructure =
    new Infrastructure(infrastructureName)

  private[templates] val stackSetEncoders =
    stackSets map (ss => new Json2StackSetEncoder(this,ss._1,ss._2))

  private[templates] var resourcesByArn
  : Map[String,Vector[Resource]] = Map()


  private[templates] var externalResources
  : Set[ExternalResource] = Set()


  logger.info(s"Initialization of $infrastructureName Infrastructure Encoder and sub-Encoders completed.")

  private def encode(): Infrastructure = {
    stackSetEncoders foreach (_.updateResourcesNames())
    logger.info("Binding Resources names, in addition to their logical IDs.")
    infrastructure.stackSets = (stackSetEncoders map (_.encode())).toSet
    logger.info("Fully instantiated all Resource objects and their properties.")
    infrastructure.externalResources = this.externalResources
    logger.info(s"Generation of Infrastructure Object for $infrastructureName completed.")
    infrastructure
  }







}