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

package com.cloud.formal.mapping.actions

import com.cloud.formal.mapping.Renaming
import com.cloud.formal.model.ModelIRI

import scala.jdk.CollectionConverters._


private object ActionsModelMapper
{


  def fromServiceActions(serviceActions: ServiceActions): ActionsModel =
    new ActionsModelMapper(serviceActions).encode()


}


private
class ActionsModelMapper(val serviceActions: ServiceActions)
{

  private val AssumeRoleService = "sts"
  private val AssumeRoleAction  = "assumerole"
  private val m = new ActionsModel(Renaming.ServActName(serviceActions.service))
  private val assumeRoleProp = m.df
    .getOWLObjectProperty(ModelIRI.actionIRI(AssumeRoleService,AssumeRoleAction))


  def encode(): ActionsModel = {
    m.ontology.add(
      serviceActions.actions
        .flatMap(actionToRole)
        .asJava)
    m
  }


  private def actionToRole(act : Action)  = {
    val actionProp =  m.df.getOWLObjectProperty(
      ModelIRI.actionIRI(serviceActions.service,act.name))

    Vector(m.df.getOWLDeclarationAxiom(actionProp)) ++
      Vector(m.df.getOWLSubPropertyChainOfAxiom(
        List(assumeRoleProp,actionProp).asJava,
        actionProp))
  }


}

