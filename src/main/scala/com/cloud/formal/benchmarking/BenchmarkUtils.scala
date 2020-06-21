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


private object BenchmarkUtils
{



  def ProfileFunction[R](name: String, timeFun: => (Long,R), warmUpFun: => Unit): (Long,R) =
  {
    println(s"\n[Benchmarking $name]")
    warmUpFun
    System.gc()
    System.runFinalization()
    val res = (1 to Runs.N).map(i => {
      if (i!=Runs.N && i%50==0){
        System.gc()
        System.runFinalization()
        //if (i%50==0) println("\tMeasured Iteration n " + i)
      }
      timeFun
    })
    val meanTime = res.foldLeft(0L)((a,e) => a + e._1)/res.size
    //println(s"\tMean Time: $meanTime")
    (meanTime,res.last._2)
  }



  def ProfileTwoFunctions[R](fun1: => Boolean,
                             fun2: => R, fun3: => R)
  :(Long,Long,R) = {

    def time(fun1: => Boolean, fun2: => R, fun3: R):
    (Long,Long,R) =
    {
      val t0 = System.nanoTime()
      val res1 = fun1
      //val t1 = System.nanoTime()
      val res2: R = if (res1) fun2 else fun3
      (0,System.nanoTime()-t0,res2)
    }

    def warmUp() :Unit =
      (1 to Runs.WN).foreach(i => {
        if (i%50==0){
          System.gc()
          System.runFinalization()
          //println("\tWarmup Iteration n " + i)
        }
        time(fun1,fun2, fun3)
      })
    warmUp()
    val res = (1 to Runs.N).map(i => {
      if (i!=Runs.N && i%50==0){
        System.gc()
        System.runFinalization()
        //println("\tMeasured Iteration n " + i)
      }
      time(fun1,fun2,fun3)
    })
    val meanTimeFun1 = res.foldLeft(0L)((a,e) => a + e._1)/res.size
    val meanTimeFun2 = res.foldLeft(0L)((a,e) => a + e._2)/res.size
    (meanTimeFun1, meanTimeFun2,res.last._3)
  }



  def time[R](fun: => R): (Long,R) =
  {
    val t0 = System.nanoTime()
    val res = fun
    (System.nanoTime-t0,res)
  }



  def warmUp[R](fun: => R): Unit =
    (1 to Runs.WN).foreach(i => {
      if (i != Runs.N && i%50==0){
        System.gc()
        System.runFinalization()
        //println("\tWarmup Iteration n " + i)
      }
      time(fun)
    })



  def timePreFun[S,R](preFun: => S, fun: S => R): (Long,R) =
  {
    val outPrefun = preFun
    val t0 = System.nanoTime()
    val res = fun(outPrefun)
    val t1 = System.nanoTime()
    println(t1-t0)
    (t1-t0,res)
  }



  def warmUpPreFun[S,R](preFun: => S, fun: S => R): Unit =
    (1 to Runs.WN).foreach(i => {
      if (i!=Runs.N && i%5==0){
        System.gc()
        System.runFinalization()
        if (i%10==0) println("\tWarmup Iteration n " + i)
      }
      timePreFun(preFun,fun)
    })





}
