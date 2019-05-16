package aws.cfn.specification

import argonaut.Json
import aws.cfn.formalization.{StackSet, Template}

object JsonStackSetDecoder {

  def decodeWithName(templates: Vector[(String,Json,Json)], stackSetName: String): StackSet = {

    //templates map ( (n,t,d) => new Template(n) )

    new StackSet(stackSetName)

    // DecodUtils.getNodesAsMapOfStrings()    Returns a Map[String,String] from a json descriptor


  }


}
