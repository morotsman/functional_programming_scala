package com.github.morotsman.func_programming.chapter2

import org.scalatest._

class FibSpec extends FlatSpec with Matchers {
   
  "Fib of 1" should "be 0" in {
    assert(Chapter2.fib(1) == 0)
  }
  
  "Fib of 2" should "be 1" in {
    assert(Chapter2.fib(2) == 1)
  }  
  
  "Fib of 3" should "be 1" in {
    assert(Chapter2.fib(3) == 1)
  }   
  
  "Fib of 4" should "be 2" in {
    assert(Chapter2.fib(4) == 2)
  } 
  
  "Fib of 5" should "be 3" in {
    assert(Chapter2.fib(5) == 3)
  }  
  
  "Fib of 6" should "be 5" in {
    assert(Chapter2.fib(6) == 5)
  }   
}
