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

}