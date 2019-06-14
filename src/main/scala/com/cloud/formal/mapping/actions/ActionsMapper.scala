package com.claudiacauli.www.cloudformal.mapping.actions

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


