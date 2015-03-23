package com.github.morotsman.func_programming.chapter8

import org.scalatest._
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
    assert(result.exists { x => x == 5 })
    assert(result.exists { x => x == 6 })
    assert(result.exists { x => x == 7 })
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
  
  "Gen.listOfN(3, unit(42))" should "result in List(42,42,42)" in { 
    val rng: RNG = SimpleRNG(10)
    val (result, rng2) = Gen.listOfN(3,Gen.unit(42)).sample.run(rng)
    assert(result == List(42,42,42))
  }  
  
  "Gen.chooseAPair(4,5)" should "result in (4,4)" in { 
    val rng: RNG = SimpleRNG(10)
    val (result, rng2) = Gen.chooseAPair(4, 5).sample.run(rng);
    assert(result == (4,4))
  } 
  
  
  "Gen.unit(10).map(_+10)" should "result in 20" in { 
    val rng: RNG = SimpleRNG(10)
    val (result, rng2) = Gen.unit(10).map(_+10).sample.run(rng);
    assert(result == 20)
  }  
  
  "Gen.unit(10).map2(Gen.unit(10))(_+_+10)" should "result in 30" in { 
    val rng: RNG = SimpleRNG(10)
    val (result, rng2) = Gen.unit(10).map2(Gen.unit(10))(_+_+10).sample.run(rng);
    assert(result == 30)
  }    

  "Gen.chooseAPairInTermsOfMap2(4,5)" should "result in (4,4)" in { 
    val rng: RNG = SimpleRNG(10)
    val (result, rng2) = Gen.chooseAPairInTermsOfMap2(4, 5).sample.run(rng);
    assert(result == (4,4))
  }   
  
  "Gen.unit(42).flatMap { x => Gen.unit(43) }" should "result in 43" in { 
    val rng: RNG = SimpleRNG(10)
    val (result, rng2) = Gen.unit(42).flatMap { x => Gen.unit(43) }.sample.run(rng);
    assert(result == 43)
  }    
  
  "Gen.choose(3, 6).listOfN(Gen.unit(1000))" should "result in a list of random 3, 5 and 6's" in { 
    val rng: RNG = SimpleRNG(10)
    val (result, rng2) = Gen.choose(3, 6).listOfN(Gen.unit(1000)).sample.run(rng);
    assert(result.size == 1000)
    assert(result.exists { x => x == 3 })
    assert(result.exists { x => x == 4 })
    assert(result.exists { x => x == 5 })
    assert(result.forall { x => x == 3 || x == 4 || x == 5 })
  }    
  
  "Gen.union(Gen.unit(1), Gen.unit(2)).listOfN(Gen.unit(1000))" should "result in a list of random 1 and 2's" in { 
    val rng: RNG = SimpleRNG(10)
    val (result, rng2) = Gen.union(Gen.unit(1), Gen.unit(2)).listOfN(Gen.unit(1000)).sample.run(rng)
    assert(result.size == 1000)
    assert(result.exists { x => x == 1 })
    assert(result.exists { x => x == 2 })
    assert(result.forall { x => x == 1 || x == 2 })
    assert(result.filter(_ == 1).size == 496)//should be near 500
  } 
  
  "Gen.weighted((Gen.unit(1),1), (Gen.unit(2),1)).listOfN(Gen.unit(1000))" should "result in a list of random 1 and 2's" in { 
    val rng: RNG = SimpleRNG(10)
    val (result, rng2) = Gen.weighted((Gen.unit(1),1), (Gen.unit(2),1)).listOfN(Gen.unit(1000)).sample.run(rng)
    assert(result.size == 1000)
    assert(result.exists { x => x == 1 })
    assert(result.exists { x => x == 2 })
    assert(result.forall { x => x == 1 || x == 2 })
    assert(result.filter(_ == 1).size == 496)//should be near 500
  } 
  
  "Gen.weighted((Gen.unit(1),0), (Gen.unit(2),1)).listOfN(Gen.unit(1000))" should "result in a list of 2's" in { 
    val rng: RNG = SimpleRNG(10)
    val (result, rng2) = Gen.weighted((Gen.unit(1),0), (Gen.unit(2),1)).listOfN(Gen.unit(1000)).sample.run(rng)
    assert(result.size == 1000)
    assert(result.forall { x => x == 2 })
  }
  
  "Gen.weighted((Gen.unit(1),1), (Gen.unit(2),0)).listOfN(Gen.unit(1000))" should "result in a list of 2's" in { 
    val rng: RNG = SimpleRNG(10)
    val (result, rng2) = Gen.weighted((Gen.unit(1),1), (Gen.unit(2),0)).listOfN(Gen.unit(1000)).sample.run(rng)
    assert(result.size == 1000)
    assert(result.forall { x => x == 1 })
  } 
  
  "Gen.weighted((Gen.unit(1),1), (Gen.unit(2),3)).listOfN(Gen.unit(1000))" should "result in a list of random 1 and 2's" in { 
    val rng: RNG = SimpleRNG(10)
    val (result, rng2) = Gen.weighted((Gen.unit(1),1), (Gen.unit(2),3)).listOfN(Gen.unit(1000)).sample.run(rng)
    assert(result.size == 1000)
    assert(result.exists { x => x == 1 })
    assert(result.exists { x => x == 2 })
    assert(result.forall { x => x == 1 || x == 2 })
    assert(result.filter(_ == 1).size == 248)//should be near 250
  }   
  
  
  
  
  
  

  
}

