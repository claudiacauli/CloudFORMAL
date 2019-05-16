package aws.cfn.dlmodel

import aws.cfn.dlmodel.terminology.{ResourceSpecificationMapper, ResourceSpecificationModel}
import aws.cfn.formalization._


object DLEncoder {


  def encode(resSpec: ResourceSpecification): ResourceSpecificationModel = {
    new ResourceSpecificationMapper(resSpec).map(resSpec)
  }


}


