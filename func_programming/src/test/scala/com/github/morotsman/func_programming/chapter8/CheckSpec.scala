package com.github.morotsman.func_programming.chapter8

import org.scalatest._
import com.github.morotsman.func_programming.chapter8.Gen
import com.github.morotsman.func_programming.chapter6.RNG
import com.github.morotsman.func_programming.chapter6.SimpleRNG
import com.github.morotsman.func_programming.chapter6.State

class CheckSpec  extends FlatSpec with Matchers{
  
   
  "Gen.choose(1,2)" should "result in 1" in { 
    val rng: RNG = SimpleRNG(10)
    val (result, rng2) = Gen.choose(1, 2).sample.run(rng);
    assert(result == 1)
  }
  
  "Gen.choose(2,3)" should "result in 2" in { 
    val rng: RNG = SimpleRNG(10)
    val (result, rng2) = Gen.choose(2, 3).sample.run(rng);
    assert(result == 2)
  }  
  
  "Gen.choose(-1,0)" should "result in -1" in { 
    val rng: RNG = SimpleRNG(10)
    val (result, rng2) = Gen.choose(-1, 0).sample.run(rng);
    assert(result == -1)
  }    
   
  
  "Gen.choose(1,3)" should "result in 1 or 2" in { 
    val rng: RNG = SimpleRNG(10)
    val choosers = List.fill(1000)(Gen.choose(1, 3).sample)
    val (result, rng2) = State.sequence(choosers).run(rng)
    assert(result.forall { x => x == 1 || x == 2 })
  }  
  
  "Gen.choose(5,8)" should "result in 5, 6, 7" in { 
    val rng: RNG = SimpleRNG(10)
    val choosers = List.fill(1000)(Gen.choose(5, 8).sample)
    val (result, rng2) = State.sequence(choosers).run(rng)
    assert(result.forall { x => x == 5 || x == 6 || x == 7 })
  }  
  
  
  "Gen.choose(-8,-5)" should "result in -8, -7, -6" in { 
    val rng: RNG = SimpleRNG(10)
    val choosers = List.fill(1000)(Gen.choose(-8, -5).sample)
    val (result, rng2) = State.sequence(choosers).run(rng)
    assert(result.forall { x => x == -8 || x == -7 || x == -6 })
  }   
  
  "Gen.unit(42)" should "result in 42" in { 
    val rng: RNG = SimpleRNG(10)
    val (result, rng2) = Gen.unit(42).sample.run(rng);
    assert(result == 42)
  }    

  
  "Gen.boolean" should "result in true or false" in { 
    val rng: RNG = SimpleRNG(10)
    val choosers = List.fill(1000)(Gen.boolean.sample)
    val (result, rng2) = State.sequence(choosers).run(rng)
    assert(result.exists { x => x == true })
    assert(result.exists { x => x == false })
    assert(result.forall { x => x == true || x ==false })
  }   

  
  
  

  
}

