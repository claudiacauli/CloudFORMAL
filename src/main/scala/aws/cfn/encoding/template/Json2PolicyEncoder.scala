package aws.cfn.encoding.template

import argonaut.Json
import aws.cfn.formalization.PolicyNode

class Json2PolicyEncoder (ssE:Json2StackSetEncoder, tE:Json2TemplateEncoder, rE:Json2ResourceEncoder, nE:Json2NodeEncoder) {

  def encode(jsonPolicy:Json): PolicyNode = {
      // TODO
    new PolicyNode(Vector())
  }

}
