package com.github.morotsman.func_programming.chapter5

import org.scalatest._

class StreamSpec  extends FlatSpec with Matchers{
  
  "Stream().headOption" should "result in None" in {
    assert(Stream().headOption == None)
  }
  
  "Stream(1,2,3).headOption" should "result in Some(1)" in {
    assert(Stream(1,2,3).headOption == Option(1))
  }  
  
  "Stream().headOption2" should "result in None" in {
    assert(Stream().headOption == None)
  }
  
  "Stream(1,2,3).headOption2" should "result in Some(1)" in {
    assert(Stream(1,2,3).headOption == Option(1))
  }    
  
  "Stream(1,2,3).toList" should "result in List(1,2,3)" in {
    assert(Stream(1,2,3).toList == List(1,2,3))
  } 
  
  "Stream().toList" should "result in Nil" in {
    assert(Stream().toList == Nil)
  } 
  
  "Stream().take(1)" should "result in Empty" in {
    assert(Stream().take(1) == Empty)
  }
  
  "Stream(1).take(0)" should "result in Stream()" in {
    assert(Stream(1).take(0).toList == List())
  }    
  
  "Stream(1).take(1)" should "result in Stream(1)" in {
    assert(Stream(1).take(1).toList == List(1))
  }   
  
  "Stream(1,2,3).take(1)" should "result in Stream(1)" in {
    assert(Stream(1,2,3).take(1).toList == List(1))
  } 
  
  "Stream(1,2,3).take(3)" should "result in Stream(1,2,3)" in {
    assert(Stream(1,2,3).take(3).toList == List(1,2,3))
  }  
  
  "Stream(1,2,3).take(4)" should "result in Stream(1,2,3)" in {
    assert(Stream(1,2,3).take(4).toList == List(1,2,3))
  }   
  
  "Stream(1,2,3).takeWhile(_ < 2)" should "result in Stream(1)" in {
    assert(Stream(1,2,3).takeWhile(_ < 2).toList == List(1))
  }  
  
  "Stream().takeWhile(_ < 2)" should "result in Stream()" in {
    assert((Stream():Stream[Int]).takeWhile(_ < 2).toList == List())
  } 
  
  "Stream(1,2,3).takeWhile(_ < 4)" should "result in Stream(1,2,3)" in {
    assert(Stream(1,2,3).takeWhile(_ < 4).toList == List(1,2,3))
  }   
  
  "Stream(1,2,3).takeWhile(_ < 1)" should "result in Stream()" in {
    assert(Stream(1,2,3).takeWhile(_ < 1).toList == List())
  }  
  
  "Stream(1,2,3).exists(_ == 2)" should "result in true" in {
    assert(Stream(1,2,3).exists(_ == 2) == true)
  }   
  
  "Stream(1,2,3).exists(_ == 4)" should "result in true" in {
    assert(Stream(1,2,3).exists(_ == 4) == false)
  } 
  
  "Stream(1,2,3).foldRight(0)(_+_)" should "result in 6" in {
    assert(Stream(1,2,3).foldRight(0)(_+_) == 6)
  } 
  
  "Stream().foldRight(0)(_+_)" should "result in 0" in {
    assert((Stream(): Stream[Int]).foldRight(0)(_+_) == 0)
  } 
  
  "Stream(1,2,3).exists2(_ == 2)" should "result in true" in {
    assert(Stream(1,2,3).exists2(_ == 2) == true)
  }   
  
  "Stream(1,2,3).exists2(_ == 4)" should "result in true" in {
    assert(Stream(1,2,3).exists2(_ == 4) == false)
  }   
  
  "Stream(1,2,3).forAll(_ > 0)" should "result in true" in {
    assert(Stream(1,2,3).forAll(_ > 0) == true)
  } 
  
  "Stream(1,2,3).forAll(_ < 3)" should "result in false" in {
    assert(Stream(1,2,3).forAll(_ < 3) == false)
  } 
  
  
  "Stream(1,2,3).takeWhile2(_ < 2)" should "result in Stream(1)" in {
    assert(Stream(1,2,3).takeWhile2(_ < 2).toList == List(1))
  }  
  
  "Stream().takeWhile2(_ < 2)" should "result in Stream()" in {
    assert((Stream():Stream[Int]).takeWhile2(_ < 2).toList == List())
  } 
  
  "Stream(1,2,3).takeWhile2(_ < 4)" should "result in Stream(1,2,3)" in {
    assert(Stream(1,2,3).takeWhile2(_ < 4).toList == List(1,2,3))
  }   
  
  "Stream(1,2,3).takeWhile2(_ < 1)" should "result in Stream()" in {
    assert(Stream(1,2,3).takeWhile2(_ < 1).toList == List())
  }   
  
  "Stream(1,2,3).map(_ + 1)" should "result in Stream(2,3,4)" in {
    assert(Stream(1,2,3).map(_ + 1).toList == List(2,3,4))
  }   
  
  "Stream().map(_ + 1)" should "result in Stream()" in {
    assert((Stream(): Stream[Int]).map(_ + 1).toList == List())
  }   
  
  "Stream(1,2,3).filter(_ < 3)" should "result in Stream(1,2)" in {
    assert(Stream(1,2,3).filter(_ < 3).toList == List(1,2))
  }   
  
  "Stream().filter(_ == 1)" should "result in Stream()" in {
    assert((Stream(): Stream[Int]).filter(_ == 1).toList == List())
  }  
  
  "Stream(1,2,3).append(Stream(4,5,6))" should "result in Stream(1,2,3,4,5,6)" in {
    assert(Stream(1,2,3).append(Stream(4,5,6)).toList == List(1,2,3,4,5,6))
  } 
  
  "Stream().append(Stream(4,5,6))" should "result in Stream(4,5,6)" in {
    assert(Stream().append(Stream(4,5,6)).toList == List(4,5,6))
  }  
  
  "Stream(1,2,3).append(Stream())" should "result in Stream(1,2,3)" in {
    assert(Stream(1,2,3).append(Stream()).toList == List(1,2,3))
  } 
  
  "Stream(1,2,3).flatMap((a) => Stream(a,a))" should "result in Stream(1,1,2,2,3,3)" in {
    assert(Stream(1,2,3).flatMap(a => Stream(a,a)).toList == List(1,1,2,2,3,3))
  }     
  
  "Stream().flatMap((a) => Stream(a,a))" should "result in Stream()" in {
    assert(Stream().flatMap(a => Stream(a,a)).toList == List())
  } 
  
  "Stream(1,2,3).find(_ == 2)" should "result in Some(2)" in {
    assert(Stream(1,2,3).find(_ == 2) == Some(2))
  }     
  
  "Stream(1,2,3).find(_ == 4)" should "result in None" in {
    assert(Stream(1,2,3).find(_ == 4) == None)
  }   
  
  "Stream.ones.take(5)" should "result in Stream(1,1,1,1,1)" in {
    assert(Stream.ones.take(5).toList == List(1,1,1,1,1))
  }  
  
  "Stream.ones().find(_ == 1)" should "result in Some(1)" in {
    assert(Stream.ones.find(_ == 1) == Some(1))
  }  
  
  "test of ones" should "result in immidiate return" in {
    Stream.ones.map(_+1).exists(_ % 2==0)
    Stream.ones.takeWhile(_ == 1)
    Stream.ones.forAll(_ != 1)
  }  

  "Stream.constant('a').take(5)" should "result in Stream('a','a','a','a','a')" in {
    assert(Stream.constant("a").take(5).toList == List("a", "a", "a", "a", "a"))
  }  
  
  "Stream.constant('a').take(0)" should "result in Stream()" in {
    assert(Stream.constant("a").take(1).toList == List("a"))
  }    
  
  /*
  "test of ones" should "result in stack overflow" in {
    Stream.ones.forAll(_ == 1)
  }    
  */
  
  "Stream.from(2).take(0)" should "result in Stream()" in {
    assert(Stream.from(2).take(0).toList == List())
  }  
  
  "Stream.from(2).take(3)" should "result in Stream(2,3,4)" in {
    assert(Stream.from(2).take(3).toList == List(2,3,4))
  }    
  
  "Stream.fibs.take(7)" should "result in Stream(0,1,1,2,3,5,8)" in {
    assert(Stream.fibs.take(7).toList == List(0,1,1,2,3,5,8))
  }     
    
  
  "test" should "result in Stream()" in {
    println("Result: " + Stream(1,2,3,4).map( a => {println(a + 10);a + 10}).filter(a => {println(a % 2 == 0); a % 2 == 0}).toList)
  } 
  
  
  "Stream.onesInTermsOfUnfold.take(5)" should "result in Stream(1,1,1,1,1)" in {
    assert(Stream.onesInTermsOfUnfold.take(5).toList == List(1,1,1,1,1))
  }
  
  "Stream.onesInTermsOfUnfold2.take(5)" should "result in Stream(1,1,1,1,1)" in {
    assert(Stream.onesInTermsOfUnfold2.take(5).toList == List(1,1,1,1,1))
  }    
  
  "Stream.constantInTermsOfUnfold('a').take(5)" should "result in Stream('a','a','a','a','a')" in {
    assert(Stream.constantInTermsOfUnfold("a").take(5).toList == List("a", "a", "a", "a", "a"))
  }  
  
  "Stream.constantInTermsOfUnfold('a').take(0)" should "result in Stream()" in {
    assert(Stream.constantInTermsOfUnfold("a").take(1).toList == List("a"))
  }   
  
  "Stream.fromInTermsOfUnfold(2).take(0)" should "result in Stream()" in {
    assert(Stream.fromInTermsOfUnfold(2).take(0).toList == List())
  }  
  
  "Stream.fromInTermsOfUnfold(2).take(3)" should "result in Stream(2,3,4)" in {
    assert(Stream.fromInTermsOfUnfold(2).take(3).toList == List(2,3,4))
  } 
  
  "Stream.fibsInTermsOfUnfold.take(7)" should "result in Stream(0,1,1,2,3,5,8)" in {
    assert(Stream.fibs.take(7).toList == List(0,1,1,2,3,5,8))
  }   
  
  
  "Stream(1,2,3).mapInTermsOfUnfold(_ + 1)" should "result in Stream(2,3,4)" in {
    assert(Stream(1,2,3).mapInTermsOfUnfold(_ + 1).toList == List(2,3,4))
  }   
  
  "Stream().mapInTermsOfUnfold(_ + 1)" should "result in Stream()" in {
    assert((Stream(): Stream[Int]).mapInTermsOfUnfold(_ + 1).toList == List())
  }   
  
  
  "Stream().takeInTermsOfUnfold(1)" should "result in Empty" in {
    assert(Stream().takeInTermsOfUnfold(1) == Empty)
  }
  
  "Stream(1).takeInTermsOfUnfold(0)" should "result in Stream()" in {
    assert(Stream(1).takeInTermsOfUnfold(0).toList == List())
  }    
  
  "Stream(1).takeInTermsOfUnfold(1)" should "result in Stream(1)" in {
    assert(Stream(1).takeInTermsOfUnfold(1).toList == List(1))
  }   
  
  "Stream(1,2,3).takeInTermsOfUnfold(1)" should "result in Stream(1)" in {
    assert(Stream(1,2,3).takeInTermsOfUnfold(1).toList == List(1))
  } 
  
  "Stream(1,2,3).takeInTermsOfUnfold(3)" should "result in Stream(1,2,3)" in {
    assert(Stream(1,2,3).takeInTermsOfUnfold(3).toList == List(1,2,3))
  }  
  
  "Stream(1,2,3).takeInTermsOfUnfold(4)" should "result in Stream(1,2,3)" in {
    assert(Stream(1,2,3).takeInTermsOfUnfold(4).toList == List(1,2,3))
  } 
  
  
  
  "Stream(1,2,3).takeWhileInTermsOfUnfold(_ < 2)" should "result in Stream(1)" in {
    assert(Stream(1,2,3).takeWhileInTermsOfUnfold(_ < 2).toList == List(1))
  }  
  
  "Stream().takeWhileInTermsOfUnfold(_ < 2)" should "result in Stream()" in {
    assert((Stream():Stream[Int]).takeWhileInTermsOfUnfold(_ < 2).toList == List())
  } 
  
  "Stream(1,2,3).takeWhileInTermsOfUnfold(_ < 4)" should "result in Stream(1,2,3)" in {
    assert(Stream(1,2,3).takeWhileInTermsOfUnfold(_ < 4).toList == List(1,2,3))
  }   
  
  "Stream(1,2,3).takeWhileInTermsOfUnfold(_ < 1)" should "result in Stream()" in {
    assert(Stream(1,2,3).takeWhileInTermsOfUnfold(_ < 1).toList == List())
  }  
  
  
  
  
  "Stream(1,2,3).zipWith(Stream(4,5,6))(_ + _)" should "result in Stream(5,7,9)" in {
    assert(Stream(1,2,3).zipWith(Stream(4,5,6))(_ + _).toList == List(5,7,9))
  }  
  
  "Stream(1,2,3).zipWith(Stream(4,5))(_ + _)" should "result in Stream(5,7)" in {
    assert(Stream(1,2,3).zipWith(Stream(4,5))(_ + _).toList == List(5,7))
  } 
  
  "Stream(1,2,3).zipWith(Stream())(_ + _)" should "result in Stream()" in {
    assert(Stream(1,2,3).zipWith(Stream())(_ + _).toList == List())
  }
  
  "Stream(1,2).zipWith(Stream(4,5,6))(_ + _)" should "result in Stream(5,7)" in {
    assert(Stream(1,2).zipWith(Stream(4,5,6))(_ + _).toList == List(5,7))
  }    
  
  "Stream().zipWith(Stream(4,5,6))(_ + _)" should "result in Stream()" in {
    assert((Stream(): Stream[Int]).zipWith(Stream(4,5,6))(_ + _).toList == List())
  }    
  
  
  
  "Stream(1,2,3).zipAll(Stream(4,5,6))" should "result in Stream((Some(1), Some(4)), (Some(2), Some(5)), (Some(3), Some(6)))" in {
    assert(Stream(1,2,3).zipAll(Stream(4,5,6)).toList == List((Some(1), Some(4)), (Some(2), Some(5)), (Some(3), Some(6))))
  }  
  
  "Stream(1,2,3).zipAll(Stream(4,5))" should "result in Stream((Some(1), Some(4)), (Some(2), Some(5)), (Some(3), None))" in {
    assert(Stream(1,2,3).zipAll(Stream(4,5)).toList == List((Some(1), Some(4)), (Some(2), Some(5)), (Some(3), None)))
  } 
  
  "Stream(1,2,3).zipAll(Stream())" should "result in Stream((Some(1), None), (Some(2), None), (Some(3), None))" in {
    assert(Stream(1,2,3).zipAll(Stream()).toList == List((Some(1), None), (Some(2), None), (Some(3), None)))
  }     
  
  "Stream(1,2).zipAll(Stream(4,5,6))" should "result in Stream((Some(1), Some(4)), (Some(2), Some(5)), (None, Some(6)))" in {
    assert(Stream(1,2).zipAll(Stream(4,5,6)).toList == List((Some(1), Some(4)), (Some(2), Some(5)), (None, Some(6))))
  } 
  
  "Stream().zipAll(Stream(4,5,6))" should "result in Stream((None, Some(4)), (None, Some(5)), (None, Some(6)))" in {
    assert(Stream().zipAll(Stream(4,5,6)).toList == List((None, Some(4)), (None, Some(5)), (None, Some(6))))
  }    
  
  "Stream().zipAll(Stream())" should "result in Stream()" in {
    assert(Stream().zipAll(Stream()).toList == List())
  } 
  
  "Stream(1,2,3).startsWith(Stream(1))" should "result in true" in {
    assert(Stream(1,2,3).startsWith(Stream(1)) == true)
  }   
  
  "Stream(1,2,3).startsWith(Stream())" should "result in true" in {
    assert(Stream(1,2,3).startsWith(Stream()) == true)
  } 
  
  "Stream(1,2,3).startsWith(Stream(1,2,3))" should "result in true" in {
    assert(Stream(1,2,3).startsWith(Stream(1,2,3)) == true)
  }  
  
  "Stream(1,2,3).startsWith(Stream(1,3))" should "result in false" in {
    assert(Stream(1,2,3).startsWith(Stream(1,3)) == false)
  }  
  
  "Stream(1,2).startsWith(Stream(1,2,3))" should "result in false" in {
    assert(Stream(1,2).startsWith(Stream(1,2,3)) == false)
  }  
  
  
  "Stream(1,2,3).tails" should "result in Stream(Stream(1,2,3), Stream(2,3), Stream(3))" in {
    assert(Stream(1,2,3).tails.map(s => s.toList).toList == List(List(1, 2, 3), List(2, 3), List(3), List()))
  } 
  
  "Stream().tails" should "result in Stream()" in {
    assert(Stream().tails.map(s => s.toList).toList == List(List()))
  }   

  
  
  
  
  

  
}

