package com.github.morotsman.func_programming.chapter3

import org.scalatest._

class OptionSpec  extends FlatSpec with Matchers{
  
  "Some(2).map(_+1)" should "result in Some(3)" in {
    assert(Some(2).map(_+1) == Some(3))
  }  
  
  "Some(2).getOrElse(3)" should "result in Some(2)" in {
    assert(Some(2).getOrElse(3) == 2)
  } 
  
  "None.getOrElse(3)" should "result in Some(3)" in {
    assert(None.getOrElse(3) == 3)
  }  
  
  
  "Some(2).flatMap(v => Some(v))" should "result in Some(2)" in {
    assert(Some(2).flatMap(v => Some(v)) == Some(2))
  }  
  
  "Some(2).orElse(Some(3))" should "result in Some(2)" in {
    assert(Some(2).orElse(Some(3)) == Some(2))
  }  
  
  "None.orElse(Some(3))" should "result in Some(2)" in {
    assert(None.orElse(Some(3)) == Some(3))
  }  
  
  "Some(2).filter(_ == 2)" should "result in Some(2)" in {
    assert(Some(2).filter(_ == 2) == Some(2))
  }  
  
  "Some(2).filter(_ != 2)" should "result in Some(2)" in {
    assert(Some(2).filter(_ != 2) == None)
  }    
  
 

  
}

