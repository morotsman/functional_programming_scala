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
    case Cons(_,_) if n == 0 => empty
    case Cons(h,_) if n == 1 => cons(h(),empty)
    case Cons(h,t) => cons(h(),t().take(n-1))
  }
  
  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(_,_) if n == 0 => this
    case Cons(h,t) => t().drop(n-1)
  }
  
  def  takeWhile(p : A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h,_) if !p(h()) => empty
    case Cons(h,t) => cons(h(), t().takeWhile(p))
  }
  
  def peek(fun: A => Unit): Stream[A] = this match {
    case Empty => Empty
    case Cons(h,t) => {
      fun(h())
      cons(h(),t().peek(fun))
    } 
  }
  
  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h,t) => p(h()) || t().exists(p)
    case _ => false
  }
  
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }
  
  def forAll(p: A => Boolean) : Boolean = 
    foldRight(true)((a,b) => p(a) && b)
  
  def takeWhileInTermsOfFoldRight(p: A => Boolean) : Stream[A] = 
    foldRight(empty: Stream[A])((a,b) => if(p(a)) cons(a,b) else empty)
    
  def headOptionInTermsOfFoldRight: Option[A] = 
    foldRight(None: Option[A])((a,b) => Some(a))
    
  def map[B](f: A => B): Stream[B] = 
    foldRight(empty: Stream[B])((a,b) => cons(f(a),b))
    
  def filter(p: A => Boolean): Stream[A] = 
    foldRight(empty: Stream[A])((a,b) => if(p(a)) cons(a,b) else b)
    
  def append[B >: A](that: => Stream[B]): Stream[B] = 
    foldRight(that)((a,b) => cons(a,b))    
    
  def flatMap[B](f: A => Stream[B]): Stream[B] = 
    foldRight(empty: Stream[B])((a,b) => f(a).append(b))
    
  def find(p: A => Boolean): Option[A] = 
    filter(p).headOption
    
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
    
  val ones: Stream[Int] = Stream.cons(1, ones)
  
  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))
    
    
}