package aws.cfn.specifications.encoding

import aws.cfn.maps.ActionsMap
import aws.cfn.specifications.formalization.{Action, ServiceActions}

object Map2ToServiceActions {


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
