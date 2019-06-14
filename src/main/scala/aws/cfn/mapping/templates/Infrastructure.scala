package aws.cfn.mapping.templates

import java.io.{File, PrintWriter}

import argonaut.Json

class Infrastructure(val name:String) {


  private[templates] var stackSets     : Set[StackSet]   = Set()
  private[templates] var aclStatements : Set[Statement]  = Set()
  private[templates] var externalResources : Set[ExternalResource] = Set()

  private type Policy = (String,String,Json,Set[Statement])

  private[templates] var policies : Vector[Policy] = Vector()


  override def toString: String = {
    "\nInfrastructure: " + name + ", includes StackSets: \n" +
      stackSets.foldLeft("")((a,b)=> a + " - " + b.name + " \n")  + "\n" +
      stackSets.foldLeft("")((a,b)=> a + b.toString + "\n")
  }


  private def printPolicies : String = {
    policies.foldLeft("")( (acc,p) => acc+
      "\n\n =====================================================" +
      "\n         STACKSET: " + p._1 + " - TEMPLATE: " + p._2 + "\n Policy: \t" +
      p._3 + "\n STATEMENTS SUMMARY: \n" +
      p._4.foldLeft("")((acc1,s)=>acc1+s.toString+"\n")
    )
  }


  def writeInfrastructureSummaryToFolder(folder: String): Unit =
    writeSummaryToFolder(
      folder,SummaryFileName.Infrastructure,this.toString)


  def writePolicySummaryToFolder(folder: String ): Unit =
    writeSummaryToFolder(
      folder,SummaryFileName.Permissions,printPolicies)


  private def writeSummaryToFolder(folder: String, suffix: String, summary: String): Unit = {
    val pw = new PrintWriter(new File(folder+name+"/"+suffix))
    pw.write(summary)
    pw.close()
  }


}
