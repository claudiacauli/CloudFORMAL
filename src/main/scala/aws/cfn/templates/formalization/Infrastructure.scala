package aws.cfn.templates.formalization

import argonaut.Json


class Infrastructure(val name:String) {

  var stackSets     : Set[StackSet]   = Set()
  var aclStatements : Set[Statement]  = Set()

  type Policy = (String,String,Json,Set[Statement])
  var policies : Vector[Policy] = Vector()

  override def toString: String = {
    "\nInfrastructure: " + name + ", includes StackSets: \n" +
      stackSets.foldLeft("")((a,b)=> a + " - " + b.name + " \n")  + "\n" +
      stackSets.foldLeft("")((a,b)=> a + b.toString + "\n")
  }

  def printPolicies : String = {
    policies.foldLeft("")( (acc,p) => acc+
      "\n\n =====================================================================================" +
      "\n         STACKSET: " + p._1 + " - TEMPLATE: " + p._2 + "\n Policy: \t" +
      p._3 + "\n STATEMENTS SUMMARY: \n" +
      p._4.foldLeft("")((acc1,s)=>acc1+s.toString+"\n")
    )
  }

}
