package aws.cfn.templates.encoding

import argonaut.Json
import aws.cfn.templates.formalization.PolicyDocument

class Json2PolicyDocumentEncoder(ssE:Json2StackSetEncoder, tE:Json2TemplateEncoder, rE:Json2ResourceEncoder, nE:Json2NodeEncoder) {

  def encode(jsonPolicy:Json): PolicyDocument = {
      // TODO

    PolicyDocument(Vector())
  }

}
