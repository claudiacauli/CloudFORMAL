package aws.cfn.templates.encoding

import aws.cfn.dlmodel.template.InfrastructureModel
import aws.cfn.templates.formalization.Infrastructure

object Infrastructure2DLEncoder {


    def encode(infrastructure: Infrastructure): InfrastructureModel = {
      new Infrastructure2DLEncoder(infrastructure).encode()
    }
}

class Infrastructure2DLEncoder(infrastructure: Infrastructure) {

  def encode(): InfrastructureModel = {
    val infrastructureModel = new InfrastructureModel(infrastructure.name,
      infrastructure.stacksets map (ss => StackSet2DLEncoder.encode(ss)))

    infrastructureModel
  }

}
