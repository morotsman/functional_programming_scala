package com.github.morotsman.func_programming.chapter3

import org.scalatest._

sealed trait Fruit
    
case class Apple() extends Fruit
    
case class Orange() extends Fruit

class NListSpec extends FlatSpec with Matchers {

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
  
  
  
  
  
  
  
   
  

  

}
