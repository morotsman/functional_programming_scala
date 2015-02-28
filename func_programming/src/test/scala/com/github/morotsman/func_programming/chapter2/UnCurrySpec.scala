package com.github.morotsman.func_programming.chapter2

import org.scalatest._

class UnCurrySpec extends FlatSpec with Matchers {

  "result of curryAdd(1)(1)" should "be equal to uncurry(curryAdd)(1,1)" in {
    val curryAdd = (a: Int) => (b: Int) => a + b
    assert(Chapter2.uncurry(curryAdd)(1, 1) == curryAdd(1)(1))
  } 
  
  "result of add(1,2)" should "be equal to uncurry(curry(add))(1,2)" in {
    val add = (a: Int, b:Int) => a + b
    assert(Chapter2.uncurry(Chapter2.curry(add))(1,2) == add(1,2))
  } 
  
  "result of curryIsSmaller(1,2)" should "be equal to uncurry(curry(isSmaller))(1, 2)" in {
    val curryIsSmaller = (a: Int) => (b:Int) => a < b
    assert(Chapter2.uncurry(curryIsSmaller)(1, 2) == curryIsSmaller(1)(2))
  }   
  

}
