package com.github.morotsman.func_programming.chapter6

import org.scalatest._
import com.github.morotsman.func_programming.chapter6._

class RNGSpec  extends FlatSpec with Matchers{
  
  "SimpleRNG(1).nextInt" should "result in (384748,SimpleRNG(25214903928L))" in {
    assert(SimpleRNG(1).nextInt == (384748,SimpleRNG(25214903928L)))
  }
  
  "nonNegativeInt" should "result in non negative ints" in {
     val (res, rng2) = SimpleRNG(1).nextInt 
     assert(rng2.nextInt._1 < 0)
     assert(RNG.nonNegativeInt(rng2)._1 > 0)
  }  
  
  "double" should "result in double" in {
     val (res: Double, rng2: RNG) = RNG.double(SimpleRNG(1)) 
  }
  
  "intsInTermsOfSequence" should "result in a list of ints" in {
     val ints = RNG.intsInTermsOfSequence(10)
     assert(ints(SimpleRNG(1))._1.length == 10)
  }     
  
  
  
  
  

  
}

