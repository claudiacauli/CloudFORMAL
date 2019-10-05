/*
 *    Copyright 2019 Claudia Cauli
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */

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
