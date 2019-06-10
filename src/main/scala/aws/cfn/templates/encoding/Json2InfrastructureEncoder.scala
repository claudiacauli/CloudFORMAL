package aws.cfn.templates.encoding

import argonaut.Json
import aws.cfn.templates.formalization.Infrastructure

object Json2InfrastructureEncoder {

  def encode(stackSets: Vector[(Vector[(String,Json,Option[Json])],String)], infrastructureName:String): Infrastructure = {
    new Json2InfrastructureEncoder(stackSets,infrastructureName).encode()
  }

}


class Json2InfrastructureEncoder(stackSets: Vector[(Vector[(String,Json,Option[Json])],String)], infrastructureName:String){

  val stackSetEncoders: Vector[Json2StackSetEncoder] = stackSets map (ss => new Json2StackSetEncoder(this,ss._1,ss._2))

  def encode(): Infrastructure = {
        new Infrastructure(infrastructureName,
          stackSetEncoders map (ssE => ssE.encode()) )
  }

}