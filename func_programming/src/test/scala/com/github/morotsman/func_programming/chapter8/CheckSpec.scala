package com.github.morotsman.func_programming.chapter8

import org.scalatest._
import java.util.concurrent._
import com.github.morotsman.func_programming.chapter6.RNG
import com.github.morotsman.func_programming.chapter6.SimpleRNG
import com.github.morotsman.func_programming.chapter6.State
import com.github.morotsman.func_programming.chapter7.Par

class CheckSpec extends FlatSpec with Matchers {

  
  "Gen.choose(1,2)" should "result in 1" in {
    val rng: RNG = SimpleRNG(10)
    val (result, rng2) = Gen.choose(1, 2).sample.run(rng);
    assert(result == 1)
  }
  
  "Gen.choose(1,20)" should "result in 1" in {
    val rng: RNG = SimpleRNG(10)
    val (result, rng2) = Gen.choose(1, 20).sample.run(rng);
    assert(result == 9)
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
    val choosers: List[State[RNG, Boolean]] = List.fill(1000)(Gen.boolean.sample)

    val rng: RNG = SimpleRNG(10)
    val (result: List[Boolean], rng2) = State.sequence(choosers).run(rng)
    assert(result.exists { x => x == true })
    assert(result.exists { x => x == false })
    assert(result.forall { x => x == true || x == false })
  }
  
  "Gen.unit(42).flatMap { x => Gen.unit(43) }" should "result in 43" in {
    val rng: RNG = SimpleRNG(10)
    val (result, rng2) = Gen.unit(42).flatMap { x:Int => Gen.unit(43) }.sample.run(rng);
    assert(result == 43)
  }
  
  "Gen.listOfN(3, unit(42))" should "result in List(42,42,42)" in {
    val rng: RNG = SimpleRNG(10)
    val tmp: Gen[List[Int]] = Gen.listOfN(3, Gen.unit(42))
    val (result, rng2) = Gen.listOfN(3, Gen.unit(42)).sample.run(rng)
    assert(result == List(42, 42, 42))
  }  
  
  "Gen.listOfN(1000,Gen.choose(3, 6))" should "result in a list of random 3, 5 and 6's" in {
    val rng: RNG = SimpleRNG(10)
    val (result, rng2) = Gen.listOfN(1000,Gen.choose(3, 6)).sample.run(rng);
    assert(result.size == 1000)
    assert(result.exists { x => x == 3 })
    assert(result.exists { x => x == 4 })
    assert(result.exists { x => x == 5 })
    assert(result.forall { x => x == 3 || x == 4 || x == 5 })
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
    assert(result.filter(_ == 1).size == 498) //should be near 500
    
  }  
  
/*








  "Gen.weighted((Gen.unit(1),1), (Gen.unit(2),1)).listOfN(Gen.unit(1000))" should "result in a list of random 1 and 2's" in {
    val rng: RNG = SimpleRNG(10)
    val (result, rng2) = Gen.weighted((Gen.unit(1), 0.5), (Gen.unit(2), 0.5)).listOfN(Gen.unit(1000)).sample.run(rng)
    assert(result.size == 1000)
    assert(result.exists { x => x == 1 })
    assert(result.exists { x => x == 2 })
    assert(result.forall { x => x == 1 || x == 2 })
    assert(result.filter(_ == 1).size == 502) //should be near 500
  }

  "Gen.weighted((Gen.unit(1),0), (Gen.unit(2),1)).listOfN(Gen.unit(1000))" should "result in a list of 2's" in {
    val rng: RNG = SimpleRNG(10)
    val (result, rng2) = Gen.weighted((Gen.unit(1), 0), (Gen.unit(2), 1)).listOfN(Gen.unit(1000)).sample.run(rng)
    assert(result.size == 1000)
    assert(result.forall { x => x == 2 })
  }

  "Gen.weighted((Gen.unit(1),1), (Gen.unit(2),0)).listOfN(Gen.unit(1000))" should "result in a list of 1's" in {
    val rng: RNG = SimpleRNG(10)
    val (result, rng2) = Gen.weighted((Gen.unit(1), 1), (Gen.unit(2), 0)).listOfN(Gen.unit(1000)).sample.run(rng)
    assert(result.size == 1000)
    assert(result.forall { x => x == 1 })
  }

  "Gen.weighted((Gen.unit(1),1), (Gen.unit(2),3)).listOfN(Gen.unit(1000))" should "result in a list of random 1 and 2's" in {
    val rng: RNG = SimpleRNG(10)
    val (result, rng2) = Gen.weighted((Gen.unit(1), 0.25), (Gen.unit(2), 0.75)).listOfN(Gen.unit(1000)).sample.run(rng)
    assert(result.size == 1000)
    assert(result.exists { x => x == 1 })
    assert(result.exists { x => x == 2 })
    assert(result.forall { x => x == 1 || x == 2 })
    assert(result.filter(_ == 1).size == 262) //should be near 250
  }

  "42" should "equal 42" in {
    val rng: RNG = SimpleRNG(10)
    val generator = Gen.unit(42);
    val prop = Prop.forAll(generator)(a => a == 42)
    val result = prop.run(1, 1, rng)
    assert(result == Prop.Passed)
  }

  "42" should "not equal 43" in {
    val rng: RNG = SimpleRNG(10)
    val generator = Gen.unit(42);
    val prop = Prop.forAll(generator)(a => a == 43)
    val result = prop.run(1, 1, rng)
    assert(result == Prop.Falsified("42", 0))
  }

  "42" should "not equal 43 2" in {
    val rng: RNG = SimpleRNG(10)
    val generator = Gen.unit(42);
    val prop = Prop.forAll(generator)(a => a == 43)
    val result = prop.run(1, 1, rng)
    assert(result == Prop.Falsified("42", 0))
  }

  "true and true" should "should pass" in {
    val rng: RNG = SimpleRNG(10)
    val trueGenerator = Gen.unit(true);
    val prop = Prop.forAll(trueGenerator)(a => a) && Prop.forAll(trueGenerator)(a => a)
    val result = prop.run(1, 1, rng)
    assert(result == Prop.Passed)
  }

  "true and false" should "should be falsified" in {
    val rng: RNG = SimpleRNG(10)
    val trueGenerator = Gen.unit(true);
    val falseGenerator = Gen.unit(false);
    val prop = Prop.forAll(trueGenerator)(a => a) && Prop.forAll(falseGenerator)(a => a)
    val result = prop.run(1, 1, rng)
    assert(result == Prop.Falsified("false", 0))
  }

  "false and true" should "should be falsified" in {
    val rng: RNG = SimpleRNG(10)
    val trueGenerator = Gen.unit(true);
    val falseGenerator = Gen.unit(false);
    val prop = Prop.forAll(falseGenerator)(a => a) && Prop.forAll(trueGenerator)(a => a)
    val result = prop.run(1, 1, rng)
    assert(result == Prop.Falsified("false", 0))
  }

  "42 || 43" should "pass 42 or 43" in {
    val rng: RNG = SimpleRNG(10)
    val generator = Gen.unit(42);
    val prop = Prop.forAll(generator)(a => a == 42) || Prop.forAll(generator)(a => a == 43)
    val result = prop.run(1, 1, rng)
    assert(result == Prop.Passed)
  }

  "43 || 42" should "pass 42 or 43" in {
    val rng: RNG = SimpleRNG(10)
    val generator = Gen.unit(42);
    val prop = Prop.forAll(generator)(a => a == 43) || Prop.forAll(generator)(a => a == 42)
    val result = prop.run(1, 1, rng)
    assert(result == Prop.Passed)
  }

  "43 || 42" should "fail at 44" in {
    val rng: RNG = SimpleRNG(10)
    val generator = Gen.unit(44);
    val prop = Prop.forAll(generator)(a => a == 43) || Prop.forAll(generator)(a => a == 42)
    val result = prop.run(1, 1, rng)
    assert(result == Prop.Falsified("44", 0))
  }

  "listOf(0)" should "generate an empty list" in {
    val rng: RNG = SimpleRNG(10)
    val zeroGenerator = Gen.choose(0, 1)
    val listGenerator = SGen.listOf(zeroGenerator)
    assert(listGenerator.forSize(0).sample.run(rng)._1 == List())
  }

  "listOf(1)" should "generate a list of one" in {
    val rng: RNG = SimpleRNG(10)
    val zeroGenerator = Gen.choose(0, 1)
    val listGenerator = SGen.listOf(zeroGenerator)
    assert(listGenerator.forSize(1).sample.run(rng)._1 == List(0))
  }

  "listOf(4)" should "generate a list of four" in {
    val rng: RNG = SimpleRNG(10)
    val intGenerator = Gen.choose(0, 10)
    val listGenerator = SGen.listOf(intGenerator)
    assert(listGenerator.forSize(4).sample.run(rng)._1 == List(4, 0, 6, 9))
  }

  //tests using the new test framework

  "the usablity of the framework" should "be acceptable 1" in {
    val smallInt = Gen.choose(-10, 10)
    val maxProp = Prop.forAll(SGen.listOf1(smallInt)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }
    Prop.run(maxProp)
  }

  "the usablity of the framework" should "be acceptable 2" in {
    val smallInt = Gen.choose(-10, 10)
    val sortedProp = Prop.forAll(SGen.listOf(smallInt)) { ns =>
      val nss = ns.sorted
      
      // We specify that every sorted list is either empty, has one element,
      // or has no two consecutive elements `(a,b)` such that `a` is greater than `b`.
      val prop1 = (nss.isEmpty || nss.tail.isEmpty || !nss.zip(nss.tail).exists {
        case (a, b) => a > b
      }) 
      val prop2 = ! ns.exists(!nss.contains(_)) 
      val prop3 =  ! nss.exists(!ns.contains(_))
      prop1 && prop2 && prop3
    }
    Prop.run(sortedProp)
  }

  "the usablity of the framework" should "be acceptable 3" in {
    val ES: ExecutorService = Executors.newCachedThreadPool
    val onePlusOneIsTwo = Prop.forAll(Gen.unit(Par.unit(1)))(i => {
      val p1 = Par.map(i)(_ + 1)(ES).get
      val p2 = Par.unit(2)(ES).get
      p1 == p2
    })
    Prop.run(onePlusOneIsTwo)
  }

  "the usablity of the framework" should "be acceptable 4" in {
    val ES: ExecutorService = Executors.newCachedThreadPool
    val onePlusOneIsTwo = Prop.check {
      val p1 = Par.map(Par.unit(1))(_ + 1)(ES).get
      val p2 = Par.unit(2)(ES).get
      p1 == p2
    }
    Prop.run(onePlusOneIsTwo)
  }
  
  "the usablity of the framework" should "be acceptable 5" in {
    val smallInt = Gen.choose(-10, 10)
    val takeWhile = Prop.forAll(SGen.listOf(smallInt)) { ns =>
      
      val prop1 = ns.takeWhile(_ => false) == List()
      val prop2 = ns.takeWhile(_ => true) == ns
      val prop3 = ns.takeWhile(_ < 0).forall(_ < 0 ) == true
      
      prop1 && prop2
      
    }
    Prop.run(takeWhile)
  }  
*/
}

