package com.github.morotsman.func_programming.chapter3
 
sealed trait List[+A] {
  
  def tail(): List[A] = this match{
    case Nil => Nil
    case Cons(hd, tl) => tl
  }
  
  @annotation.tailrec
  final def drop(n: Int): List[A] = this match{
    case Nil => this
    case Cons(hd,tl) => tl.drop(n-1)
  }
  
  @annotation.tailrec
  final def dropWhile(f:  A => Boolean): List[A] = this match {
    case Nil => Nil
    case Cons(hd, tl) if f(hd)=> tl.dropWhile(f); 
    case _ => this
  }
  
  def append[B >: A](a2: List[B]): List[B] = this match {
    case Nil => a2
    case Cons(hd,tl) => Cons(hd, tl.append(a2))
  }
  
  def init(): List[A] = this match {
    case Nil => Nil
    case Cons(hd, Nil) => Nil
    case Cons(hd,tl) => Cons(hd, tl.init()) 
  }
  
  def setHead[B >: A](head: B): List[B] = this match{
    case Nil => sys.error("not possible on empty list")
    case Cons(_,tl) => Cons(head,tl) 
  }
  
  def foldRight[B](z: B)(f: (A,B) => B): B = this match{
    case Nil => z
    case Cons(hd,tl) => f(hd, tl.foldRight(z)(f))
  }
  
  def length2() : Int = this.foldRight(0)((_,acc) => acc + 1)
  def length() : Int = this.foldLeft(0)((acc,_) => acc + 1)
  
  def foldLeft[B](z: B)(f: (B,A) => B): B = {
    @annotation.tailrec
    def go(as: List[A], acc: B):B = as match{
      case Nil => acc
      case Cons(hd,tl) => go(tl, f(acc, hd))
    }
    
    go(this, z)   
  }
  
  def reverse(): List[A] = 
    this.foldLeft(Nil: List[A])((acc,a) => Cons(a,acc))
    
  def foldRightInTermOfFoldLeft[B](z: B)(f: (A,B) => B): B =
    this.reverse.foldLeft(z)((a, acc) => f(acc,a))
    
  def foldLeftInTermOfFoldRight[B](z: B)(f: (B,A) => B): B = 
    this.reverse.foldRight(z)((acc,a) => f(a,acc))  
    
  def appendInTermsOfFoldLeft[B >: A](a2: List[B]): List[B] = 
    this.reverse.foldLeft(a2)((acc, a) => Cons(a,acc))
    

    
    
    
 
  
}

case object Nil extends List[Nothing]

case class Cons[+A](head: A, theTail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def sum(ints: List[Int]): Int = ints match {
    case Nil         => 0
    case Cons(hd, tl) => hd + sum(tl)
  }
  
  def product(ints: List[Int]): Int = ints match {
    case Nil         => 0
    case Cons(hd, tl) => hd * sum(tl)
  }  
  
  def product2(ints: List[Int]): Int = 
    ints.foldRight(0)(_ * _)
    
  def sum2(ints: List[Int]): Int = 
    ints.foldLeft(0)(_ + _)
    
  def product3(ints: List[Int]): Int = 
    ints.foldLeft(1)(_ * _)
    
  def concat[A](l : List[List[A]]): List[A] = 
    l.foldLeft(Nil:List[A])((acc,a) => acc.append(a))

}