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

private class Action(val name: String)
private class ServiceActions(val service: String, val actions: Set[Action])

private object ActionsMapper
{


  private[actions] def fromServiceName(serviceName: String) =
    toServiceActions(serviceName)


  private def toServiceActions(serviceName: String)= {
    ActionsMap.getActionPrefixes.toSet
      .map( aPref =>
        new ServiceActions( aPref,
          toActionObjects(ActionsMap.lookupActionPrefix(aPref)))
      )
  }


  private def toActionObjects(servActions: Set[String])=
    servActions map (act => new Action(act.toLowerCase))


}


