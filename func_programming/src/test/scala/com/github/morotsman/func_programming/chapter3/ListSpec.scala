package com.github.morotsman.func_programming.chapter3

import org.scalatest._

sealed trait Fruit
    
case class Apple() extends Fruit
    
case class Orange() extends Fruit

class ListSpec extends FlatSpec with Matchers {

  "List()" should "be equal to Nil" in {
    assert(List() == Nil)
  }
  
  "List(1)" should "be equal to Cons(1, Nil)" in {
    assert(List(1) == Cons(1, Nil))
  } 
  
  "List" should "be covariant" in {
    val listOfApple : List[Apple] = List(Apple())    
    val listOfOrange : List[Orange] = List(Orange())
    val listOfFruit : List[Fruit] = List(Apple(), Orange())
    val anotherListOfFruit : List[Fruit] =  Cons(Orange(), listOfApple)
    assert(true)
  }  
  
  
  "List().tail()" should "result in Nil" in {
    assert(List().tail() == Nil)
  }  
  
  "List(1).tail()" should "result in Nil" in {
    assert(List(1).tail() == Nil)
  }    
  
  "List(1,2).tail()" should "result in List(2)" in {
    assert(List(1,2).tail() == List(2))
  } 
  
  "List(1,2).setHead(4)" should "result in List(4,2)" in {
    assert(List(1,2).setHead(4) == List(4,2))
  }    
  
  "List(Apple(), Apple()).setHead(Orange())" should "result in List(Orange(),Apple())" in {
    val listOfApples : List[Apple] = List(Apple(), Apple())
    val listOfFruit : List[Fruit] = listOfApples.setHead(Orange())
    assert(listOfFruit == List(Orange(),Apple()))
  }    
  
  "List().drop(1)" should "result in Nil" in {
    assert(List().drop(1) == Nil)
  }  
  
  "List(1).drop(4)" should "result in Nil" in {
    assert(List(1).drop(4) == Nil)
  }    
  
  "List(1,2,3).drop(2)" should "result in List(3)" in {
    assert(List(1,2,3).drop(3) == Nil)
  }  
  
  "List(1).dropWhile(_<3)" should "result in Nil" in {
    assert(List(1).dropWhile(_ < 3) == Nil)
  }  
  
  "List(1,2,3).dropWhile(_ < 4)" should "result in Nil" in {
    assert(List(1,2,3).dropWhile(_ < 4) == Nil)
  }
  
  "List(1,2,3,4).dropWhile(_ < 4)" should "result in List(4)" in {
    assert(List(1,2,3,4).dropWhile(_ < 4) == List(4))
  }  
  
  "List().append(List())" should "result in Nil" in {
    assert(List().append(List()) == Nil)
  } 
  
  "List().append(List(1,2))" should "result in List(1,2)" in {
    assert(List().append(List(1,2)) == List(1,2))
  }
  
  "List(1,2).append(List())" should "result in List(1,2)" in {
    assert(List(1,2).append(List()) == List(1,2))
  }  
  
  "List(1,2).append(List(3,4,5))" should "result in List(1,2,3,4,5)" in {
    assert(List(1,2).append(List(3,4,5)) == List(1,2,3,4,5))
  }  
  
  "List().init()" should "result in Nil" in {
    assert(List().init() == Nil)
  }   
  
  "List(1).init()" should "result in Nil" in {
    assert(List(1).init() == Nil)
  }  
  
  "List(1,2).init()" should "result in List(1)" in {
    assert(List(1,2).init() == List(1))
  } 
  
  "List(1,2,3).init()" should "result in List(1,2)" in {
    assert(List(1,2,3).init() == List(1,2))
  }  
   
  
  "List(1,2,3).foldRight(0)(_ + _)" should "result in 6" in {
    assert(List(1,2,3).foldRight(0)(_ + _) == 6)
  }   
  
  "List(1,2,3).foldRight(Nil:List[Int])(Cons(_,_))" should "result in List(1,2,3)" in {
    assert(List(1,2,3).foldRight(Nil:List[Int])(Cons(_,_)) == List(1,2,3))
  }   
  
  "List(1,2,3).foldRightInTermOfFoldLeft(0)(_ + _)" should "result in 6" in {
    assert(List(1,2,3).foldRightInTermOfFoldLeft(0)(_ + _) == 6)
  }   
  
  "List(1,2,3).foldRightInTermOfFoldLeft(Nil:List[Int])(Cons(_,_))" should "result in List(1,2,3)" in {
    assert(List(1,2,3).foldRightInTermOfFoldLeft(Nil:List[Int])(Cons(_,_)) == List(1,2,3))
  }     
  
  "List().length" should "result in 0" in {
    assert(List().length == 0)
  } 
  
  "List(1,2,3).length" should "result in 3" in {
    assert(List(1,2,3).length == 3)
  }  
  
  "List(1,2,3).foldLeft(0)(_ + _)" should "result in 6" in {
    assert(List(1,2,3).foldLeft(0)(_ + _) == 6)
  }   
  
  "List(1,2,3).foldLeft(Nil:List[Int])(Cons(_,_))" should "result in List(3,2,1)" in {
    assert(List(1,2,3).foldLeft(Nil:List[Int])((acc,a) =>Cons(a,acc)) == List(3,2,1))
  }  
  
  "List(1,2,3).foldLeftInTermOfFoldRight(0)(_ + _)" should "result in 6" in {
    assert(List(1,2,3).foldLeftInTermOfFoldRight(0)(_ + _) == 6)
  }   
  
  "List(1,2,3).foldLeftInTermOfFoldRight(Nil:List[Int])(Cons(_,_))" should "result in List(3,2,1)" in {
    assert(List(1,2,3).foldLeftInTermOfFoldRight(Nil:List[Int])((acc,a) =>Cons(a,acc)) == List(3,2,1))
  }    
  
  "List(1,2,3).reverse" should "result in List(3,2,1)" in {
    assert(List(1,2,3).reverse == List(3,2,1))
  }   
  
  "List().reverse" should "result in List()" in {
    assert(List().reverse == List())
  }  
  
  
  "List().appendInTermsOfFoldLeft(List())" should "result in Nil" in {
    assert(List().appendInTermsOfFoldLeft(List()) == Nil)
  } 
  
  "List().appendInTermsOfFoldLeft(List(1,2))" should "result in List(1,2)" in {
    assert(List().appendInTermsOfFoldLeft(List(1,2)) == List(1,2))
  }
  
  "List(1,2).appendInTermsOfFoldLeft(List())" should "result in List(1,2)" in {
    assert(List(1,2).appendInTermsOfFoldLeft(List()) == List(1,2))
  }  
  
  "List(1,2).appendInTermsOfFoldLeft(List(3,4,5))" should "result in List(1,2,3,4,5)" in {
    assert(List(1,2).appendInTermsOfFoldLeft(List(3,4,5)) == List(1,2,3,4,5))
  }   
  
  "List.concat(List())" should "result in List(Nil)" in {
    assert(List.concat(List()) == Nil)
  }   
  
  "List.concat(List(List(1,2))" should "result in List(1,2)" in {
    assert(List.concat(List(List(1,2))) == List(1,2))
  }  
  
  "List.concat(List(List(1,2), List(3,4))" should "result in List(1,2,3,4)" in {
    assert(List.concat(List(List(1,2),List(3,4))) == List(1,2,3,4))
  } 
  
  "List(1,2).map(_+10)" should "result in List(11,12)" in {
    assert(List(1,2).map(_ + 10) == List(11,12))
  }  
  
  "List(1,2).map(_+'')" should "result in List('11','12')" in {
    assert(List(1,2).map(_ + "") == List("1","2"))
  } 
  
  "List(1,2,3).filter(_!=2)" should "result in List(1,3)" in {
    assert(List(1,2,3).filter(_ != 2) == List(1,3))
  } 
  
  "List(1,2,3).filter2(_!=2)" should "result in List(1,3)" in {
    assert(List(1,2,3).filter2(_ != 2) == List(1,3))
  }     
  
  "List(1,2,3).filter3(_!=2)" should "result in List(1,3)" in {
    assert(List(1,2,3).filter3(_ != 2) == List(1,3))
  }  
  
  "List(1,2,3).filterInTermsOfFlatMap(_!=2)" should "result in List(1,3)" in {
    assert(List(1,2,3).filterInTermsOfFlatMap(_ != 2) == List(1,3))
  }     
  
  "List(1,2,3).flatMap(a => List(a,a)" should "result in List(1,1,2,2,3,3)" in {
    assert(List(1,2,3).flatMap(a => List(a,a)) == List(1,1,2,2,3,3))
  } 
  
  "List(1,2,3).zipWith(List(1,2))(_ + _)" should "result in List(2,4)" in {
    assert(List(1,2,3).zipWith(List(1,2))(_ + _) == List(2,4))
  }   
  
  "List(1,2).zipWith(List(1,2,3))(_ + _)" should "result in List(2,4)" in {
    assert(List(1,2).zipWith(List(1,2,3))(_ + _) == List(2,4))
  }  
  
  "List().hasSubsequence(List())" should "result in true" in {
    assert(List().hasSubsequence(List()) == true)
  }  
  
  "List(1).hasSubsequence(List(1))" should "result in true" in {
    assert(List(1).hasSubsequence(List(1)) == true)
  } 
  
  "List(1).hasSubsequence(List(1,2))" should "result in false" in {
    assert(List(1).hasSubsequence(List(1,2)) == false)
  }    
  
  "List(1).hasSubsequence(List(2))" should "result in false" in {
    assert(List(1).hasSubsequence(List(2)) == false)
  }   
  
  "List(1,2).hasSubsequence(List(1))" should "result in true" in {
    assert(List(1,2).hasSubsequence(List(1)) == true)
  }  
  
  "List(1,2).hasSubsequence(List(2))" should "result in true" in {
    assert(List(1,2).hasSubsequence(List(2)) == true)
  }
  
  "List(1,2,3,4).hasSubsequence(List(2,3))" should "result in true" in {
    assert(List(1,2,3,4).hasSubsequence(List(2,3)) == true)
  } 
  
  "List(1,2,3,4).hasSubsequence(List(3,4))" should "result in true" in {
    assert(List(1,2,3,4).hasSubsequence(List(3,4)) == true)
  }   
  
  "List(1,2,3,4).hasSubsequence(List(2,4))" should "result in false" in {
    assert(List(1,2,3,4).hasSubsequence(List(2,4)) == false)
  }  
  
  
  
  
  
  
  
  
   
  

  

}
