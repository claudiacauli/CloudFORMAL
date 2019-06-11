package aws.cfn.specifications.encoding

import aws.cfn.maps.ActionsMap
import aws.cfn.specifications.formalization.{Action, ServiceActions}

object Map2ToServiceActions {


  def fromMap() : Vector[ServiceActions] = {
    ActionsMap.getKeys.toVector flatMap serviceActions
  }

  private def serviceActions(serviceName : String): Vector[ServiceActions] = {

    def resourceActionsVector (resType: String, resActions: Vector[String]) : Vector[Action] =
      resActions flatMap (act => Vector(new Action(act.toLowerCase(), resType.toLowerCase() )))

    ActionsMap.lookUp(serviceName) flatMap ( p =>
      Vector( new ServiceActions (p._1, p._2.toVector flatMap ( e =>  resourceActionsVector(e._1, e._2) ) ))
      )

  }






}
