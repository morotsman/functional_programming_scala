package com.github.morotsman.func_programming.chapter3

import org.scalatest._

class TreeSpec  extends FlatSpec with Matchers{
  
  "Leaf(1).size()" should "result in 1" in {
    assert(Leaf(1).size == 1)
  }  

  "Branch(Leaf(1),Leaf(2)).size()" should "result in 3" in {
    assert(Branch(Leaf(1), Leaf(2)).size == 3)
  } 
  
  "maximum(Branch(Leaf(1),Leaf(2)))" should "result in 2" in {
    assert(Tree.maximum(Branch(Leaf(1), Leaf(2))) == 2)
  }  
  
  "maximum(Leaf(1))" should "result in 1" in {
    assert(Tree.maximum(Leaf(1)) == 1)
  } 
  
  "maximum(Branch(Leaf(4),Branch(Leaf(2), Leaf(3))))" should "result in 2" in {
    assert(Tree.maximum(Branch(Leaf(4),Branch(Leaf(2), Leaf(3)))) == 4)
  }  
  
  "maximum(Branch(Leaf(1),Branch(Leaf(2), Leaf(4))))" should "result in 2" in {
    assert(Tree.maximum(Branch(Leaf(1),Branch(Leaf(2), Leaf(4)))) == 4)
  }    
  
  "Branch(Leaf(1),Branch(Leaf(2), Leaf(4))).depth" should "result in 3" in {
    assert(Branch(Leaf(1),Branch(Leaf(2), Leaf(4))).depth == 3)
  } 
  
  "Leaf(1).depth" should "result in 1" in {
    assert(Leaf(1).depth == 1)
  }    
  
  "Leaf(1).map(_ + 10)" should "result in Leaf(11)" in {
    assert(Leaf(1).map(_ + 10) == Leaf(11))
  }  
  
  "Branch(Leaf(1),Branch(Leaf(2), Leaf(4))).map(_ + 10)" should "result in Branch(Leaf(11),Branch(Leaf(12), Leaf(14)))" in {
    assert(Branch(Leaf(1),Branch(Leaf(2), Leaf(4))).map(_ + 10) == Branch(Leaf(11),Branch(Leaf(12), Leaf(14))))
  } 
  
  "Leaf(1).sizeInTermsOfFold()" should "result in 1" in {
    assert(Leaf(1).sizeInTermsOfFold == 1)
  }  
  
  "Branch(Leaf(1),Leaf(2)).sizeInTermsOfFold()" should "result in 3" in {
    assert(Branch(Leaf(1), Leaf(2)).sizeInTermsOfFold == 3)
  } 
  
  "Branch(Leaf(1),Branch(Leaf(2), Leaf(4))).depthInTermsOfFold" should "result in 3" in {
    assert(Branch(Leaf(1),Branch(Leaf(2), Leaf(4))).depthInTermsOfFold == 3)
  } 
  
  "Leaf(1).depthInTermsOfFold" should "result in 1" in {
    assert(Leaf(1).depthInTermsOfFold == 1)
  }   
  
  "maximumInTermsOfFold(Branch(Leaf(1),Leaf(2)))" should "result in 2" in {
    assert(Tree.maximumInTermsOfFold(Branch(Leaf(1), Leaf(2))) == 2)
  }   
  
  "maximumInTermsOfFold(Leaf(1))" should "result in 1" in {
    assert(Tree.maximumInTermsOfFold(Leaf(1)) == 1)
  } 
  
  "maximumInTermsOfFold(Branch(Leaf(4),Branch(Leaf(2), Leaf(3))))" should "result in 2" in {
    assert(Tree.maximumInTermsOfFold(Branch(Leaf(4),Branch(Leaf(2), Leaf(3)))) == 4)
  }  
  
  "maximumInTermsOfFold(Branch(Leaf(1),Branch(Leaf(2), Leaf(4))))" should "result in 2" in {
    assert(Tree.maximumInTermsOfFold(Branch(Leaf(1),Branch(Leaf(2), Leaf(4)))) == 4)
  }     
  
  "Leaf(1).mapInTermsOfFold(_ + 10)" should "result in Leaf(11)" in {
    assert(Leaf(1).mapInTermsOfFold(_ + 10) == Leaf(11))
  }  
  
  "Branch(Leaf(1),Branch(Leaf(2), Leaf(4))).mapInTermsOfFold(_ + 10)" should "result in Branch(Leaf(11),Branch(Leaf(12), Leaf(14)))" in {
    assert(Branch(Leaf(1),Branch(Leaf(2), Leaf(4))).mapInTermsOfFold(_ + 10) == Branch(Leaf(11),Branch(Leaf(12), Leaf(14))))
  }   
  
}

