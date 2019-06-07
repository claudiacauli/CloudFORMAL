package aws.cfn.templates.encoding

import argonaut.Json
import aws.cfn.templates.formalization.Infrastructure

object Json2InfrastructureEncoder {

  def encode(stackSets: Vector[(Vector[(String,Json,Option[Json])],String)], infrastructureName:String): Infrastructure = {
    new Json2InfrastructureEncoder(stackSets,infrastructureName).encode()
  }

}


class Json2InfrastructureEncoder(stackSets: Vector[(Vector[(String,Json,Option[Json])],String)], infrastructureName:String){

  def encode(): Infrastructure = {
        new Infrastructure(infrastructureName,
          stackSets map ( ss => Json2StackSetEncoder.encode(ss._1,ss._2)) )
  }

}