package aws.cfn.specifications.encoding

import aws.cfn.maps.ActionsMap
import aws.cfn.specifications.formalization.{Action, ServiceActions}

object Map2ToServiceActions {

  def fromMap() : Set[ServiceActions] = {
    ActionsMap.getKeys.toSet map serviceActions
  }

  private def serviceActions(serviceName : String): ServiceActions = {

    def toActionObjects (servActions: Set[String]) : Set[Action] =
      servActions map (act => new Action(act.toLowerCase))

    new ServiceActions( serviceName, toActionObjects( ActionsMap.lookUpServiceName(serviceName) )  )
  }






}
