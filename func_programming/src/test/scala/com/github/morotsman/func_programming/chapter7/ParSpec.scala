package com.github.morotsman.func_programming.chapter7

import org.scalatest._
import java.util.concurrent._

class ParSpec  extends FlatSpec with Matchers{
   import Par._
  
  val es:ExecutorService  = Executors.newFixedThreadPool(10); 
   
  "asyncF(a*2)(10) " should "result in 20" in {
    val asyncTimesTwo = asyncF((a:Int) => a*2)
    assert(asyncTimesTwo(10)(es).get == 20)
  }  
  
  "sequence(List(lazyUnit(10), lazyUnit(20), lazyUnit(30)))" should "result in 50" in {
    assert(sequence(List(lazyUnit(10), lazyUnit(20), lazyUnit(30)))(es).get == List(10,20,30))
  } 
  
  "parMap(List(1,2,3))(_*10)" should "result in List(10,20,30)" in {
    assert(parMap(List(1,2,3))(_*10)(es).get == List(10,20,30))
  }  
   
  "parFilter(List(1,2,3))(_<3)" should "result in List(1,2)" in {
    assert(parFilter(List(1,2,3))(_<3)(es).get == List(1,2))
  }   
  
   /*
   
  "sortPar(fork(List(4,3,6,1)))" should "result in List(1,3,4,6)" in {    
    assert(sortPar(fork(unit(List(4,3,6,1))))(es).get == List(1,3,4,6))
  }
   
  
  
  
  "map(lazyUnit(10))(_*3)" should "result in 30" in {
    assert(map(lazyUnit(10))(_*3)(es).get == 30)
  }  
   
  "map(map2(lazyUnit(10), lazyUnit(40))(_+_))" should "result in 50" in {
    assert(map2(lazyUnit(10), lazyUnit(40))(_+_)(es).get == 50)
  } 
  
 
  
  "max(List(1,2,3))(_<3)" should "result in 3" in {
    val list = List(1,2,3);
    assert(reduce(list.head)(list.toIndexedSeq)((a, b) => if(a < b) b else a)(es).get == 3)
  }    
  

  */
  
  

  
}

