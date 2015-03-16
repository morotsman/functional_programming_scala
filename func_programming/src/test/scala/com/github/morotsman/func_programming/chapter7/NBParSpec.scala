package com.github.morotsman.func_programming.chapter7

import org.scalatest._
import java.util.concurrent._

class NBParSpec  extends FlatSpec with Matchers{
   import NBPar._
  
  val es:ExecutorService  = Executors.newFixedThreadPool(3); 
   

  "asyncF(a*2)(10) " should "result in 20" in {
    val asyncTimesTwo = asyncF((a:Int) => a*2)
    assert(NBPar.run(es)(asyncTimesTwo(10)) == 20)
  }   
  
  
  "map(lazyUnit(10))(_*3)" should "result in 30" in {
    assert(NBPar.run(es)(map(lazyUnit(10))(_*3)) == 30)
  }  
   
  "map(map2(lazyUnit(10), lazyUnit(40))(_+_))" should "result in 50" in {
    assert(NBPar.run(es)(map2(lazyUnit(10), lazyUnit(40))(_+_)) == 50)
  } 
  
  
  "sequence(List(lazyUnit(10), lazyUnit(20), lazyUnit(30)))" should "result in 50" in {
    assert(NBPar.run(es)(sequence(List(lazyUnit(10), lazyUnit(20), lazyUnit(30)))) == List(10,20,30))
  } 
  

  
  "parMap(List(1,2,3))(_*10)" should "result in List(10,20,30)" in {
    assert(NBPar.run(es)(parMap(List(1,2,3))(_*10)) == List(10,20,30))
    
    /*
    println("Starting...")
    val startTime = System.currentTimeMillis()
    val expr = NBPar.parMap(List.fill(400000)(10000))(Math.sqrt(_))
    val res =  NBPar.run(es)(expr)
    
    println("time: " + (System.currentTimeMillis()-startTime))
    
    println("Starting again...")
    val startTime2 = System.currentTimeMillis()
    val expr2 = List.fill(400000)(10000).map(Math.sqrt(_))
    
    println("time: " + (System.currentTimeMillis()-startTime2))   
     
    */
    
  }  
  
  "parFilter(List(1,2,3))(_<3)" should "result in List(1,2)" in {
    assert(NBPar.run(es)(parFilter(List(1,2,3))(_<3)) == List(1,2))
  }  
  
  "max(List(1,2,3))(_<3)" should "result in 3" in {
    val list = List(1,2,3);
    assert(NBPar.run(es)(reduce(list.head)(list.toIndexedSeq)((a, b) => if(a < b) b else a)) == 3)
  }    
  

  
  
  

  
}

