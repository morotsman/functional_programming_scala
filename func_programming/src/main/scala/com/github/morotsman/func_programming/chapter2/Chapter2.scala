package com.github.morotsman.func_programming.chapter2

object Chapter2 {
  
  def fib(n: Int): Int = {

    @annotation.tailrec
    def go(nr: Int, acc1: Int, acc2: Int): Int = 
      if(nr == n) acc1
      else go(nr+1, acc2, acc1 + acc2)   
    
    return go(1, 0, 1);
  }  
  
  
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {

    @annotation.tailrec
    def loop(nr: Int): Boolean = 
      if(nr == as.length) true
      else if(!ordered(as(nr-1),as(nr))) false
      else loop(nr+1)
      
    loop(1)
  }
  
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = 
    a => b => f(a,b)
  
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = 
    (a,b) => f(a)(b)
    
  def compose[A,B,C](f: B => C, g: A => B): A => C = 
    a => f(g(a))
  

}