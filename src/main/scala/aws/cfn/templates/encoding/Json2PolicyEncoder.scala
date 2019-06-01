package aws.cfn.templates.encoding

import argonaut.Json
import aws.cfn.templates.formalization.Policy

class Json2PolicyEncoder (ssE:Json2StackSetEncoder, tE:Json2TemplateEncoder, rE:Json2ResourceEncoder, nE:Json2NodeEncoder) {

  def encode(jsonPolicy:Json): Policy = {
      // TODO

    new Policy(Vector())
  }

}
