package com.github.morotsman.func_programming.chapter5

sealed trait Stream[+A] {
  

  
  def toList: List[A] = {
    @annotation.tailrec
    def loop(acc: List[A], s: Stream[A]): List[A] = s match {
      case Empty => acc.reverse
      case Cons(hd, tl) => loop(hd()::acc, tl())
    }
    loop(Nil, this)
  }
  
  def take(n: Int): Stream[A] = this match {
    case Cons(hd,tl) if n > 1 => Cons(hd, () => tl().take(n-1))
    case Cons(hd, tl) => Cons(hd, () => Empty)
    case _ => Empty
  }
  
  def takeWhile(implicit p: A => Boolean): Stream[A] = this match {
    case Cons(hd, tl) if p(hd()) => Cons(hd, () => tl().takeWhile)
    case Cons(hd, tl) => Empty
    case _ => Empty
  }
  
  
  def exists(implicit p: A => Boolean) : Boolean = this match {
    case Cons(hd, tl) => p(hd()) || tl().exists
    case _ => false
  } 
  
  def foldRight[B](z: => B)(implicit f: (A, => B) => B): B = this match {
    case Cons(hd, tl) => f(hd(), tl().foldRight(z))
    case _ => z
  }
  
  def map[B](f: A => B): Stream[B] = 
    this.foldRight(Empty: Stream[B])((a,b) => Cons(() => f(a), () => b))
  
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }  
  
  def headOption2: Option[A] =
    this.foldRight(None: Option[A])((a, b) => Some(a))
  
  def exists2(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b)
    
  def forAll(p: A => Boolean): Boolean = 
    foldRight(true)((a, b) => p(a) && b)
    
  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, b) => if(p(a)) Cons(() => a, () => b) else Empty)
    
  def filter(p: A => Boolean): Stream[A] = 
    foldRight(Empty: Stream[A])((a, b) => if(p(a)) Cons(() => a, () => b) else b)

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