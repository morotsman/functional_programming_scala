package com.github.morotsman.func_programming.chapter4

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
  
  "Some(2).flatMap2(v => Some(v))" should "result in Some(2)" in {
    assert(Some(2).flatMap2(v => Some(v)) == Some(2))
  }    
  
  "Some(2).orElse(Some(3))" should "result in Some(2)" in {
    assert(Some(2).orElse(Some(3)) == Some(2))
  }  
  
  "None.orElse(Some(3))" should "result in Some(2)" in {
    assert(None.orElse(Some(3)) == Some(3))
  }  
  
  "Some(2).orElse2(Some(3))" should "result in Some(2)" in {
    assert(Some(2).orElse2(Some(3)) == Some(2))
  }  
  
  "None.orElse2(Some(3))" should "result in Some(2)" in {
    assert(None.orElse2(Some(3)) == Some(3))
  }   
  
  "Some(2).filter(_ == 2)" should "result in Some(2)" in {
    assert(Some(2).filter(_ == 2) == Some(2))
  }  
  
  "Some(2).filter(_ != 2)" should "result in Some(2)" in {
    assert(Some(2).filter(_ != 2) == None)
  } 
  
  
  "Some(2).filter2(_ == 2)" should "result in Some(2)" in {
    assert(Some(2).filter2(_ == 2) == Some(2))
  }  
  
  "Some(2).filter2(_ != 2)" should "result in Some(2)" in {
    assert(Some(2).filter2(_ != 2) == None)
  }    
  
  "Some(2).filter3(_ == 2)" should "result in Some(2)" in {
    assert(Some(2).filter3(_ == 2) == Some(2))
  }  
  
  "Some(2).filter3(_ != 2)" should "result in Some(2)" in {
    assert(Some(2).filter3(_ != 2) == None)
  }
  
  def variance(xs: Seq[Double]): Option[Double] = {
    val meanOfSeq = if(xs.length == 0) None else Some(xs.sum/xs.length)    
    meanOfSeq.map(m => xs.map(x => math.pow(x-m,2))).map(s => s.sum/s.length)
  }
  
  "map2(Some(2), Some(3))(_ + _)" should "result in Some(5)" in {
    assert(Option.map2(Some(2), Some(3))(_ + _) == Some(5))
  }  
  
  "map2(Some(2), None)(_ + _)" should "result in Some(5)" in {
    assert(Option.map2(Some(2), None)(_ + _) == None)
  }  
  
  "map2(None, Some(3))(_ + _)" should "result in Some(5)" in {
    assert(Option.map2(None : Option[Int], Some(3))(_ + _) == None)
  } 
  
  "sequence(List(Some(2), Some(3), Some(4)))" should "result in Some(List(2,3,4)" in {
    assert(Option.sequence(List(Some(2), Some(3), Some(4))) == Some(List(2,3,4)))
  } 
  
  "sequence(List(None, Some(3), Some(4)))" should "result in None" in {
    assert(Option.sequence(List(None, Some(3), Some(4))) == None)
  }   
  
  "sequence(List(Some(2), Some(3), None))" should "result in None" in {
    assert(Option.sequence(List(Some(2), Some(3), None)) == None)
  }   
  
  "traverse(List(2, 3, 4))(a => if(a==5) None else Some(a) )" should "result in Some(List(2,3,4))" in {
    assert(Option.traverse(List(2, 3, 4))(a => if(a==5) None else Some(a) ) == Some(List(2,3,4)))
  }   
  
  "traverse(List(2, 5, 4))(a => if(a==5) None else Some(a) )" should "result in None" in {
    assert(Option.traverse(List(2, 5, 4))(a => if(a==5) None else Some(a) ) == None)
  }    
    
  
 

  
}

