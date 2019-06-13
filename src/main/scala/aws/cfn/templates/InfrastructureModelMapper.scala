package aws.cfn.templates

import aws.cfn.dlmodel.Model

protected object InfrastructureModelMapper {

    def encode(infrastructure: Infrastructure): Model = {
      new InfrastructureModelMapper(infrastructure).encode()
    }
}

private class InfrastructureModelMapper(infrastructure: Infrastructure) {

  def encode(): InfrastructureModel = {

    val infrastructureModel = new InfrastructureModel(
        infrastructure.stackSets map (ss => StackSetModelMapper.encode(ss,infrastructure)),
        infrastructure)

    infrastructureModel
  }

}
