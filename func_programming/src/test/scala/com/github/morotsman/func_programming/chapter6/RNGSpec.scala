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
  
  
  "coin when locked" should "unlocked machine" in {
     val simulation = Machine.simulateMachine(List(Coin))
     val result = simulation.run(Machine(true, 10, 0))
     assert(result._1 == (1, 10))
     assert(result._2 == Machine(false, 10, 1))
  }
  
  "coin when unlocked" should "do nothing" in {
     val simulation = Machine.simulateMachine(List(Coin, Coin))
     val result = simulation.run(Machine(true, 10, 0))
     assert(result._1 == (1, 10))
     assert(result._2 == Machine(false, 10, 1))
  }   
  
  "turn when locked" should "do nothing" in {
     val simulation = Machine.simulateMachine(List(Turn))
     val result = simulation.run(Machine(true, 10, 0))
     assert(result._1 == (0, 10))
     assert(result._2 == Machine(true, 10, 0))
  }  
  
  "turn when unlocked" should "dispense candy" in {
     val simulation = Machine.simulateMachine(List(Coin, Turn))
     val result = simulation.run(Machine(true, 10, 0))
     assert(result._1 == (1, 9))
     assert(result._2 == Machine(true, 9, 1))
  }  
  
  "a machine with no candy" should "ignore input" in {
     val simulation = Machine.simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn))
     val result = simulation.run(Machine(true, 1, 0))
     assert(result._1 == (1, 0))
     assert(result._2 == Machine(true, 0, 1))
  }  
  
  "10 coins and 5 candies" should "result in (14,1) if 4 candies are bought" in {
     val simulation = Machine.simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn))
     val result = simulation.run(Machine(true, 5, 10))
     assert(result._1 == (14, 1))
     assert(result._2 == Machine(true, 1, 14))
  }   
  
  
  

  
}

