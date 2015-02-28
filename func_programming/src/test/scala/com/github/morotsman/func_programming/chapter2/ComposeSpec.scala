package com.github.morotsman.func_programming.chapter2

import org.scalatest._

class ComposeSpec extends FlatSpec with Matchers {

  "result of compose(add20, add10)(5)" should "be equal 35" in {
    val add10 = (a: Int) => a + 10
    val add20 = (b:Int) => b + 20
    assert(Chapter2.compose(add20, add10)(5) == 35)
  } 
  
  "result of compose(toMessage, add10)(5)" should "be equal to 'Result is: 15'" in {
    val add10 = (a: Int) => a + 10
    val toMessage = (b:Int) => "Result is: " + b
    assert(Chapter2.compose(toMessage, add10)(5) == "Result is: 15")
  }   
  

  

}
