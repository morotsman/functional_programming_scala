package com.github.morotsman.func_programming.chapter2

object Chapter2 {
  
  def fib(n: Int): Int = {
    
    def go(n: Int): Int = n match{
      case 1 => 0
      case 2 => 1 
      case _ => go(n-1) + go(n-2)
    }
    
    go(n)
  }
  
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
     def go(i: Int): Boolean = {
       if( i >= as.length - 1) true
       else if (ordered(as(i), as(i+1))) go(i+1)
       else false    
     } 
     
     go(0)
  }
  
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    (a:A) => (b:B) => f(a,b)
  }
  
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }
  
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))  
  }
  

}