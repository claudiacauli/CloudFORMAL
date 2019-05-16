package aws.cfn.encoding.template

import aws.cfn.dlmodel.template.StackSetModel
import aws.cfn.formalization.StackSet

object StackSetDLEncoder {

  def encode(stackSet: StackSet): StackSetModel ={
    new StackSetDLEncoder(stackSet).encode()
  }

}


class StackSetDLEncoder(stackSet: StackSet){

  val m : StackSetModel = new StackSetModel(stackSet.name)

  def encode(): StackSetModel ={

    /*
    TODO Populate the model m will the set of all axioms computed by instantiating all the things in the StackSet object!
     */

    m
  }

  /*
  TODO Write necessary encoding submethods, that correspond to encoding of all different parts of a template
   */

}
