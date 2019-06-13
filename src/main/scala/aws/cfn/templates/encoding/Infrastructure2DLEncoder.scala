package aws.cfn.templates.encoding

import aws.cfn.dlmodel.template.{InfrastructureModel, PermissionsModel}
import aws.cfn.templates.formalization.Infrastructure

object Infrastructure2DLEncoder {


    def encode(infrastructure: Infrastructure): (InfrastructureModel,PermissionsModel) = {
      new Infrastructure2DLEncoder(infrastructure).encode()
    }
}

class Infrastructure2DLEncoder(infrastructure: Infrastructure) {

  def encode(): (InfrastructureModel,PermissionsModel) = {

    val infrastructureModel = new InfrastructureModel(
        infrastructure.name,
        infrastructure.stackSets map (ss => StackSet2DLEncoder.encode(ss,infrastructure)),
        infrastructure)
    val permissionsModel    = Statements2DLEncoder.encode(
      infrastructure.name, infrastructure.aclStatements,infrastructure)

    (infrastructureModel, permissionsModel)
  }

}
