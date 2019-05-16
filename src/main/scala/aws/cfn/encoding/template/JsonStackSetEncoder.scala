package aws.cfn.encoding.template

import argonaut.Json
import aws.cfn.formalization.StackSet

object JsonStackSetEncoder {

  // each template is (name, templateJson, descriptorJson)
  def encode(templates: Vector[(String,Json,Json)], stackSetName: String): StackSet = {
    new JsonStackSetEncoder(templates, stackSetName).encode()
  }

}



private class JsonStackSetEncoder(templates: Vector[(String,Json,Json)], stackSetName: String) {

  def encode(): StackSet = {
  null // TODO
  }


}
