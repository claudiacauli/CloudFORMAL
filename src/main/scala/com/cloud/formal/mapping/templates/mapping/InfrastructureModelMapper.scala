package com.cloud.formal.mapping.templates.mapping

import com.cloud.formal.mapping.templates.Infrastructure
import com.cloud.formal.model.Model
import com.typesafe.scalalogging.StrictLogging

object InfrastructureModelMapper
{
    def encode(infrastructure: Infrastructure): Model =
      new InfrastructureModelMapper(infrastructure)
        .encode()

}

private class InfrastructureModelMapper(infrastructure: Infrastructure)
extends StrictLogging
{

  def encode(): InfrastructureModel =
  {
    logger.info(s"Mapping ${infrastructure.name} Infrastructure to an InfrastructureModel.")
    val infrastructureModel = new InfrastructureModel(
        infrastructure.stackSets
          .map (ss =>
            StackSetModelMapper.encode(ss,infrastructure)),
        infrastructure)
    logger.info(s"${infrastructure.name} InfrastructureModel generated.")
    infrastructureModel
  }

}
