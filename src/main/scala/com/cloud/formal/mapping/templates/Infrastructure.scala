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

package com.cloud.formal.mapping.templates

import java.io.{File, PrintWriter}

class Infrastructure(val name:String) {


  private[templates] var stackSets     : Set[StackSet]   = Set()
  private[templates] var externalResources : Set[ExternalResource] = Set()



  override def toString: String = {
    "\nInfrastructure: " + name + ", includes StackSets: \n" +
      stackSets.foldLeft("")((a,b)=> a + " - " + b.name + " \n")  + "\n" +
      stackSets.foldLeft("")((a,b)=> a + b.toString + "\n")
  }



  def writeInfrastructureSummaryToFolder(folder: String): Unit =
    writeSummaryToFolder(
      folder,SummaryFileName.Infrastructure,this.toString)




  private def writeSummaryToFolder(folder: String, suffix: String, summary: String): Unit = {
    val pw = new PrintWriter(new File(folder+name+"/"+suffix))
    pw.write(summary)
    pw.close()
  }


  private[formal]
  def getResourcesCount =
    stackSets.foldLeft(0)((a,s) => a+s.getResourcesCount)


  private[formal]
  def getResourceTypesCount =
    stackSets.foldLeft(Set[String]())((a,s) => a ++ s.getResourceTypes).size



}
