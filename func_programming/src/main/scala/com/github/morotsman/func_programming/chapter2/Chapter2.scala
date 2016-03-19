package com.github.morotsman.func_programming.chapter2

object Chapter2 {
  
  def fib(n: Int): Int = 
    if (n == 0 || n==1){
      0
    }else if(n == 2){
      1
    } else  {
      fib(n-1) + fib(n-2)
    }    
  
  
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
     
    def go(n:Int) : Boolean =  {
      if (n > as.length-2) {
        true
      }else if(!ordered (as(n), as(n+1))){
        false
      } else{
        go(n+1)
      }
      
    }
    
    go(0)   
  }
  
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = 
    (a:A) => (b:B) => f(a,b)
  
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = 
    (a:A, b:B) => f(a)(b)
    
  def compose[A,B,C](f: B => C, g: A => B): A => C = 
    (a:A) => f(g(a))
  

}