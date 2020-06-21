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

package com.cloud.formal

package object benchmarking
{

  private[benchmarking]
  object Runs
  {
    val WN = 10
    val N  = 1
  }

  private[benchmarking]
  object CSV
  {
    val FilePath = "src/main/scala/com/cloud/formal/benchmarking/table.csv"
    val Id      = 0
    val Nr      = 1
    val Nrt     = 2
    val EncT    = 3
    val Naxs    = 4
    val ClassT  = 5
    val Usat    = 6
    val Sat0    = 7
    val Sat1    = 8
  }



}
