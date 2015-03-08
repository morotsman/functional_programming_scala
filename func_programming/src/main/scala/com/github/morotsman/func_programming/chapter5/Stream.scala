package com.github.morotsman.func_programming.chapter5

import Stream._

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
    case Cons(hd,tl) if n > 0 => Cons(hd, () => tl().take(n-1))
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
    
  def append[B >: A](s: => Stream[B]): Stream[B] = 
    foldRight(s)((a, b) => cons(a, b))
    
  def flatMap[B](f: A => Stream[B]): Stream[B] = 
    foldRight(Empty: Stream[B])((a, b) => f(a).append(b))
    
  def find(p: A => Boolean): Option[A] = 
    filter(p).headOption
    
    
  def mapInTermsOfUnfold[B](f: A => B): Stream[B] = 
    unfold(this)(s => s match {
      case Empty => None
      case Cons(hd, tl) => Some((f(hd()), tl()))
    })
    
  def takeInTermsOfUnfold(n: Int): Stream[A] = 
    unfold((this, n))(v => v._1 match{
      case Cons(hd, tl) if v._2 > 0 => Some(hd(), (tl(), n-1))
      case _ => None
    })
    
  def takeWhileInTermsOfUnfold(p: A => Boolean): Stream[A] = 
    unfold(this)(s => s match {
      case Cons(hd, tl) if p(hd()) => Some(hd(), tl()) 
      case _ => None
    })
    
  def zipWith[B, C](ls: Stream[B])(f: (A,B) => C): Stream[C] = 
    unfold((this, ls))(s => (s._1, s._2) match{
      case (Empty, _) => None
      case (_, Empty) => None
      case (Cons(hd1, tl1), Cons(hd2, tl2)) => Some((f(hd1(), hd2()), (tl1(), tl2())))
    })
    
  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = 
    unfold((this, s2))(v => (v._1, v._2) match {
      case (Empty, Empty) => None
      case (Cons(hd1, tl1), Cons(hd2, tl2)) => Some(((Some(hd1()), Some(hd2())), (tl1(), tl2())))
      case (Cons(hd, tl), Empty) => Some(((Some(hd()), None), (tl(), Empty)))
      case (Empty, Cons(hd,tl)) => Some(((None, Some(hd())), (Empty, tl())))
    })
    
  def startsWith[A](s: Stream[A]): Boolean = 
    !this.zipAll(s).exists(v => (v._1, v._2) match {
      case (None, Some(_)) => true
      case (Some(v1), Some(v2)) => v1 != v2
      case _ => false
    })
    
  def tails: Stream[Stream[A]] = 
    unfold(this)(s => s match {
      case Empty => None
      case Cons(hd,tl) => Some((s, tl()))
    }).append(Stream(empty))
    
  def hasSubSequence[A](s: Stream[A]): Boolean =
    this.tails.exists(_.startsWith(s))
  
    
    
    

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
    
  def ones: Stream[Int] = cons(1, ones)
  
  def constant[A](implicit a: A): Stream[A] = 
    cons(a, constant)
    
  def from(n: Int): Stream[Int] = 
    cons(n , from(n+1))
  
  def fibs: Stream[Int] = {
    def go(a: Int,b: Int): Stream[Int] = {
       cons(a+b, go(b,a+b)) 
    } 
    
    cons(0, cons(1,go(0,1)))
      
  }
  
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match{
    case None => Empty
    case Some((a,s)) => cons(a, unfold(s)(f)) 
  }
  
  def unfold2[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = 
    f(z).map(v => cons(v._1, unfold(v._2)(f))).getOrElse(Empty)
 
    
  def onesInTermsOfUnfold = unfold(1)(_ => Some((1,1)))
  
  def onesInTermsOfUnfold2 = unfold2(1)(_ => Some((1,1)))
  
  def constantInTermsOfUnfold[A](implicit a: A): Stream[A] =
    unfold(a)(a => Some(a,a))
    
  def fromInTermsOfUnfold(n: Int): Stream[Int] =
    unfold(n)(n => Some(n,n+1))
    
  def fibsInTermsOfUnfold: Stream[Int] = 
    unfold((0,1))(n => Some((n._1, (n._2,n._1 + n._2))))
    
  
}