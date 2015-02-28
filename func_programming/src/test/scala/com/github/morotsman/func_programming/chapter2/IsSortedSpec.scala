package com.github.morotsman.func_programming.chapter2

import org.scalatest._

class IsSortedSpec extends FlatSpec with Matchers {

  "Array(1)" should "be sorted" in {
    assert(Chapter2.isSorted(Array(1), (a: Int,b: Int) => a <=b) == true)
  } 
  
  "Array(1,2)" should "be sorted" in {
    assert(Chapter2.isSorted(Array(1,2), (a: Int,b: Int) => a <=b) == true)
  }   
  
  "Array(2,1)" should "not be sorted" in {
    assert(Chapter2.isSorted(Array(2,1), (a: Int,b: Int) => a <=b) == false)
  }    
  
  
  "Array(1,2,3)" should "be sorted" in {
    assert(Chapter2.isSorted(Array(1,2,3), (a: Int,b: Int) => a <=b) == true)
  }
  
  "Array(1,3,3)" should "be sorted" in {
    assert(Chapter2.isSorted(Array(1,3,3), (a: Int,b: Int) => a <=b) == true)
  } 
  
  "Array(1,4,3)" should "not be sorted" in {
    assert(Chapter2.isSorted(Array(1,4,3), (a: Int,b: Int) => a <=b) == false)
  }    
  
  "Array(1,2,3,4,5,6)" should "be sorted" in {
    assert(Chapter2.isSorted(Array(1,2,3,4,5,6), (a: Int,b: Int) => a <=b) == true)
  }   
  
  "Array(1,2,3,5,4,6)" should "not be sorted" in {
    assert(Chapter2.isSorted(Array(1,2,3,5,4,6), (a: Int,b: Int) => a <=b) == false)
  }     
 
}
