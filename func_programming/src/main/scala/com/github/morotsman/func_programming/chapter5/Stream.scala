package com.github.morotsman.func_programming.chapter5

import Stream._

sealed trait Stream[+A] {

  def headOption: Option[A] = this match {
    case Empty      => None
    case Cons(h, t) => Some(h())
  }

  def toList: List[A] = this match {
    case Empty      => List()
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Empty                => this
    case Cons(_, _) if n == 0 => empty
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case Cons(h, t)           => cons(h(), t().take(n - 1))
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty                => Empty
    case Cons(_, _) if n == 0 => this
    case Cons(h, t)           => t().drop(n - 1)
  }
  
  def tail: Stream[A] = this.drop(1)

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty                 => Empty
    case Cons(h, _) if !p(h()) => empty
    case Cons(h, t)            => cons(h(), t().takeWhile(p))
  }

  def peek[U](fun: A => U): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => {
      fun(h())
      cons(h(), t().peek(fun))
    }
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _          => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _          => z
  }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhileInTermsOfFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty: Stream[A])((a, b) => if (p(a)) cons(a, b) else empty)

  def headOptionInTermsOfFoldRight: Option[A] =
    foldRight(None: Option[A])((a, b) => Some(a))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty: Stream[B])((a, b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty: Stream[A])((a, b) => if (p(a)) cons(a, b) else b)

  def append[B >: A](that: => Stream[B]): Stream[B] =
    foldRight(that)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty: Stream[B])((a, b) => f(a).append(b))

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption
    
  def mapInTermsOfUnfold[B](f: A => B): Stream[B] = 
    unfold(this)(s => s.headOption.map(h => (f(h), s.tail)))
    
  def takeInTermsOfUnfold(n: Int): Stream[A] = 
    unfold(this,n)(s => if (s._2 == 0) None else s._1.headOption.map(h => (h,(s._1.tail,s._2-1))))
    
  def takeWhileInTermsOfUnfold(p: A => Boolean): Stream[A] = 
    unfold(this)(s => s.headOption.flatMap(a => if(p(a)) Some((a,s.tail)) else None))
    
  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] = 
    unfold(this,s2)(streams =>  for(
        h1 <- streams._1.headOption;
        h2 <- streams._2.headOption
        ) yield (f(h1,h2),(streams._1.tail, streams._2.tail)))
        
  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = 
    unfold(this,s2)({
      case (Empty,Cons(h,t)) =>  Some(((None,Some(h())), (empty, t())))
      case (Cons(h,t), Empty) =>  Some(((Some(h()), None), (t(), empty)))
      case (Cons(h1,t1), Cons(h2,t2)) =>  Some(((Some(h1()), Some(h2())), (t1(), t2())))
      case _ => None
    })
    
  def startsWith[A](s2: Stream[A]): Boolean = 
    !zipAll(s2).takeWhile(_._2 != None).exists(v => v._1 != v._2) 
    
  def tails: Stream[Stream[A]] = 
    unfold(this)(s => if(s == Empty) None else Some(s, s.tail)).append(Stream(Stream())) 
    
  def hasSubsqequence[A](s: Stream[A]): Boolean = 
    tails exists (_ startsWith s)  
  
  def scanLeft[B](z: => B)(f: (=> B, A) => B): Stream[B] = 
    cons(z,unfold(this, z)(s => s._1.headOption.flatMap(h => Some((f(s._2,h), (s._1.tail, f(s._2,h))))))) 
    
  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = this match {
    case Empty => Stream[B](z)
    case Cons(h,t) => {
      val right@Cons(newZ,_) = t().scanRight(z)(f)
      val b = f(h(),newZ())
      cons(b, right)
    }
  }
  
  def scanRightInTermsOfFoldRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
    foldRight(Stream(z))((a,sb) => {
      val right@Cons(b,_) = sb
      cons(f(a,b()), right)
    })

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
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](implicit a: A): Stream[A] = Stream.cons(a, constant)

  def from(n: Int): Stream[Int] =
    Stream.cons(n, from(n + 1))

  def fibs = {
    def go(p: (Int, Int)): Stream[Int] = Stream.cons(p._1, go((p._2, p._1 + p._2)))

    go((0, 1))
  }
  
  def unfold[A,S](z: S)(f: S => Option[(A,S)]): Stream[A] = 
    f(z).map(v => cons(v._1, unfold(v._2)(f))).getOrElse(empty)
    
  def onesInTermsOfUnfold: Stream[Int] = unfold(1)(v => Some(v,v))
  
  def constantInTermsOfUnfold[A](a: A): Stream[A] = unfold(a)(a => Some(a,a)) 
  
  def fromInTermsOfUnfold(n: Int): Stream[Int] = unfold(n)(n => Some(n,n+1))
  
  def fibsInTermsOfUnfold: Stream[Int] = unfold((0,1))(v => Some(v._1, (v._2, v._1 + v._2)))


  

}