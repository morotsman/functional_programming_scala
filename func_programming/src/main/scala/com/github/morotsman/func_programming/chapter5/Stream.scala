package com.github.morotsman.func_programming.chapter5

import Stream._

sealed trait Stream[+A] {
  
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h,t) => Some(h())
  }
  
  def toList: List[A] = this match {
    case Empty => List()
    case Cons(h,t) => h()::t().toList
  }
  
  def take(n: Int): Stream[A] = this match {
    case Empty => this
    case Cons(_,_) if n == 0 => Empty
    case Cons(h,t) => cons(h(),t().take(n-1))
  }
  
  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(_,_) if n == 0 => this
    case Cons(h,t) => t().drop(n-1)
  }
  
  def  takeWhile(p : A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h,_) if !p(h()) => Empty
    case Cons(h,t) => cons(h(), t().takeWhile(p))
  }
  
  def peek(fun: A => Unit): Stream[A] = this match {
    case Empty => Empty
    case Cons(h,t) => {
      fun(h())
      cons(h(),t().peek(fun))
    } 
  }
  
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  
  def empty[A]: Stream[A] = Empty
  
  def apply[A](as: A*): Stream[A] = 
    if(as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
    
    
}