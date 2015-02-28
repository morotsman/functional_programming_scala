package com.github.morotsman.func_programming

import org.scalatest._

class HelloSpec extends FlatSpec with Matchers {
  "Hello" should "have tests" in {
    true should be === true
  }
}
