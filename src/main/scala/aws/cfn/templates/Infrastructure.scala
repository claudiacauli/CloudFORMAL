package aws.cfn.templates

import java.io.{File, PrintWriter}

import argonaut.Json


class Infrastructure(val name:String) {

  var stackSets     : Set[StackSet]   = Set()
  var aclStatements : Set[Statement]  = Set()
  var externalResources : Set[ExternalResource] = Set()

  type Policy = (String,String,Json,Set[Statement])
  var policies : Vector[Policy] = Vector()

  override def toString: String = {
    "\nInfrastructure: " + name + ", includes StackSets: \n" +
      stackSets.foldLeft("")((a,b)=> a + " - " + b.name + " \n")  + "\n" +
      stackSets.foldLeft("")((a,b)=> a + b.toString + "\n")
  }

  private def printPolicies : String = {
    policies.foldLeft("")( (acc,p) => acc+
      "\n\n =====================================================================================" +
      "\n         STACKSET: " + p._1 + " - TEMPLATE: " + p._2 + "\n Policy: \t" +
      p._3 + "\n STATEMENTS SUMMARY: \n" +
      p._4.foldLeft("")((acc1,s)=>acc1+s.toString+"\n")
    )
  }

  def writeInfrastructureSummaryToFolder( destinationFolder:String ) : Unit =
  {
    val pw = new PrintWriter(
      new File(destinationFolder+name+"/"+SummaryFileName.Infrastructure))
    pw.write(this.toString)
    pw.close()
  }

  def writePolicySummaryToFolder( destinationFolder:String ) : Unit =
  {
    val pw = new PrintWriter(
      new File(destinationFolder+name+"/"+SummaryFileName.Permissions))
    pw.write(printPolicies)
    pw.close()
  }


}
