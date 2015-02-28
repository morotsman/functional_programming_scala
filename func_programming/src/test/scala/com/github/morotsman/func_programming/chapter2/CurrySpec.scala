package com.github.morotsman.func_programming.chapter2

import org.scalatest._

class CurrySpec extends FlatSpec with Matchers {

  "result of add(1,1)" should "be equal to curry(add)(1)(1)" in {
    val add = (a: Int, b:Int) => a + b
    assert(Chapter2.curry(add)(1)(1) == add(1,1))
  } 
  
  "result of add(1,2)" should "be equal to curry(add)(1)(2)" in {
    val add = (a: Int, b:Int) => a + b
    assert(Chapter2.curry(add)(1)(2) == add(1,2))
  } 
  
  "result of isSmaller(1,2)" should "be equal to curry(isSmaller)(1)(2)" in {
    val isSmaller = (a: Int, b:Int) => a < b
    assert(Chapter2.curry(isSmaller)(1)(2) == isSmaller(1,2))
  }   
  

}
