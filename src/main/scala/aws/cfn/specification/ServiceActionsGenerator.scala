package aws.cfn.specification

import aws.cfn.specification.maps.ActionsMap
import aws.cfn.formalization.{Action, ServiceActions}

object ServiceActionsGenerator {




  def fromMap() : Vector[ServiceActions] = {
    ActionsMap.getKeys.toVector flatMap serviceActions
  }





  private def serviceActions(serviceName : String): Vector[ServiceActions] = {

    def resourceActionsVector (n: String, v: Vector[String]) : Vector[Action] =
      v flatMap (s => Vector(new Action(s.toLowerCase(), n.toLowerCase() )))

    ActionsMap.lookUp(serviceName) match {
      case None => Vector()
      case Some(m) => Vector( new ServiceActions (serviceName.toLowerCase(), m.toVector flatMap ( e =>  resourceActionsVector(e._1, e._2) ) ) )
    }

  }





}
