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
  
  def each(a : A => Unit): Unit = this match{
    case Empty => 
    case Cons(hd,tl) => {
      a(hd())
      tl().each(a)
    }
  }  
  
  def take(n: Int): Stream[A] = this match {
    
    case Cons(h,t) if n == 1 => cons(h(),Empty)
    case Cons(h,t) if(n > 1)=> cons(h(), t().take(n-1))
    case _ => Empty
  }
  
  def drop(n:Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h,t) if n == 0 => this
    case Cons(h,t) => t().drop(n-1)
  }
  
  def takeWhile(implicit p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h,t) if !p(h()) => Empty
    case Cons(h,t) => cons(h(),t().takeWhile)
  }
  
  
  def exists(implicit p: A => Boolean) : Boolean = 
    foldRight(false)((a,b) => p(a) || b)
  
  def foldRight[B](z: => B)(implicit f: (A, => B) => B): B = this match {
    case Cons(hd, tl) => f(hd(), tl().foldRight(z))
    case _ => z
  }
  
  def forAll(p: A => Boolean): Boolean = 
    foldRight(true)((a,b) => p(a) && b)
    
  def headOptionInTermsOfFoldRight: Option[A] =
    foldRight(None:Option[A])((a,b) => Some(a))
  
  def map[B](f: A => B): Stream[B] =
    foldRight(Empty:Stream[B])((a,b) => cons(f(a),b))
    
   def mapWithPatternMatch[B](f: A => B): Stream[B] = this match {
    case Empty => Empty
    case Cons(hd,tl) => cons(f(hd()), tl().map(f))
  }
    
  def filter(p: A => Boolean): Stream[A] = 
    foldRight(Empty:Stream[A])((a,b) => {
      if(p(a)){
        cons(a,b)
      }else{
        b
      }
    })
    
  def append[B >: A](s: => Stream[B]): Stream[B] = 
    foldRight(s)((a,b) => cons(a,b))
  
  def flatMap[B](f: A => Stream[B]): Stream[B] = 
    foldRight(Empty:Stream[B])((a,b) => f(a).append(b))
    
  def peek(f: A => Unit):Stream[A] = this match {
    case Empty => Empty
    case Cons(h,t) => {
      f(h())
      cons(h(), t().peek(f))
    }
  }
  
  def find(p: A => Boolean): Option[A] = 
    filter(p).headOption  
    
  
    
  def mapInTermsOfUnfold[B](f: A => B): Stream[B] = 
    unfold(this)(s => s.headOption.map(a => (f(a), s.drop(1))))
    
  def takeInTermsOfUnfold(n: Int): Stream[A] = 
    unfold((this,n))(s => {
      if (s._2 == 0) {
        None
      }else {
        s._1.headOption.map(a => (a,(s._1.drop(1),n-1)))
      }
    })
    
  def takeWhileInTermsOfUnfold(p: A => Boolean): Stream[A] = 
    unfold(this)(s => {
      s match {
        case Empty => None
        case Cons(hd,tl) if(!p(hd())) => None
        case Cons(hd,tl) => Some((hd(),tl()))
      }
    })
    
  def zipWith[B, C](ls: Stream[B])(f: (A,B) => C): Stream[C] = 
    unfold((this, ls))(s => {
      (s._1,s._2) match  {
        case (Cons(hd_1,tl_1), Cons(hd_2, tl_2)) => {
          val v = f(hd_1(),hd_2())
          val s = (tl_1(),tl_2())
          Some((v,s))
        }
        case _ => None
      }  
        
    })
    
 def zip[B](s2: Stream[B]): Stream[(A,B)] =
  zipWith(s2)((_,_))   
  
  
  
  

  
  
  
    
    
    
  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = 
        unfold((this, s2))(s => {
      (s._1,s._2) match  {
        case (Empty, Empty) => None
        case (Cons(hd,tl),Empty) => {
          val v = (Some(hd()), None)  
          val s = (tl(),Empty)
          Some((v,s))
        }
        case (Empty, Cons(hd,tl)) => {
          val v = (None,Some(hd()))
          val s = (Empty,tl())
          Some((v,s))
        }
        case (Cons(hd_1,tl_1), Cons(hd_2, tl_2)) => {
          val v = (Some(hd_1()), Some(hd_2()))
          val s = (tl_1(),tl_2())
          Some((v,s))
        }
      }
        
    })
    

    
    
  def startsWith[A](s: Stream[A]): Boolean = 
    zipAll(s).takeWhile(!_._2.isEmpty) forAll {case (h,h2) => h == h2}

    
    
  def tails: Stream[Stream[A]] = 
    unfold(this)((s) => s match{
      case Empty => None
      case _ => Some(s, s.drop(1))
      
    }).append(Stream(Stream()))
    
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
  
  def constant[A](a: A): Stream[A] = 
    cons(a,constant(a))
    
  def from(n: Int): Stream[Int] = 
    cons(n,from(n+1))
  
  def fibs: Stream[Int] = {

      def go(a:Int,b:Int): Stream[Int] =
        cons(a+b, go(b,a+b))
     
      cons(0, cons(1,go(0,1)))
  }

    
  
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z).map(r => cons(r._1, unfold(r._2)(f))).getOrElse(empty)
  }
    
  
 
    
  def onesInTermsOfUnfold = 
    unfold(1)(s => Some((1,1)))
    
  
  def constantInTermsOfUnfold[A](implicit a: A): Stream[A] = 
    unfold(a)(s => Some((a,a)))
    
  def fromInTermsOfUnfold(n: Int): Stream[Int] = 
    unfold(n)(s => Some((s,s+1)))
    
  def fibsInTermsOfUnfold: Stream[Int] = 
    unfold((0,1))(s => Some(s._1, (s._2, s._1 + s._2)))
    
  
}