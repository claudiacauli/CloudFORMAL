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

package com.cloud.formal.benchmarking

import java.io.File

object LatexTablePrinter {


  def print(): Unit =
  {
    val csvFile = new File(CSV.FilePath)
    if (!csvFile.exists())
      Console.print("The CSV File does not exist. Cannot print latex table.")
    else {
      val br = io.Source.fromFile(csvFile)
      val table = br.getLines().foldLeft("")((a,l) => a + "\n" + rowFromLine(l))
      br.close()
      Console.print(table)
    }
  }


  private def rowFromLine(l: String) =
  {
    val values = l.split(",")
    f"\\textbf{${values(0)}}&${values(1)}&${values(2)}&${values(3)}&" +
    f"${values(4)}&${values(5)}&${values(6)}&${values(7)}&${values(8)}\\\\"
  }

}
