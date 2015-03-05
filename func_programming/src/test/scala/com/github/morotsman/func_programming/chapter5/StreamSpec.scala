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
  
  

  
}

