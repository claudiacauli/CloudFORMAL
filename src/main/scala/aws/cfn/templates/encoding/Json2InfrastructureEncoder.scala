package aws.cfn.templates.encoding

import argonaut.Json
import aws.cfn.templates.formalization.{Infrastructure, Node}

object Json2InfrastructureEncoder {

  def encode(stackSets: Vector[(Vector[(String,Json,Option[Json])],String)], infrastructureName:String): Infrastructure = {
    new Json2InfrastructureEncoder(stackSets,infrastructureName).encode()
  }

}


class Json2InfrastructureEncoder(stackSets: Vector[(Vector[(String,Json,Option[Json])],String)], infrastructureName:String){

  val stackSetEncoders: Vector[Json2StackSetEncoder] = stackSets map (ss => new Json2StackSetEncoder(this,ss._1,ss._2))
  var resourceByArn: Map[String,Node] = Map()

  def encode(): Infrastructure = {
    val infr = new Infrastructure(infrastructureName,
          stackSetEncoders map (ssE => ssE.encode()) )
    println(this)
    infr
  }


  override def toString: String = {
    "\nInfrastructure: " + infrastructureName + ", includes StackSets: \n" +
      stackSetEncoders.foldLeft("")((a,b)=> a + " - " + b.stackSet.name + " \n")  + "\n" +
      stackSetEncoders.foldLeft("")((a,b)=> a + b.toString + "\n")
  }


}