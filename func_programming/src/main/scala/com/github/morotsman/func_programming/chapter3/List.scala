package com.github.morotsman.func_programming.chapter3

sealed trait List[+A] {

  def tail(): List[A] = this match {
    case Nil => Nil
    case Cons(x,xs) => xs
  }

  @annotation.tailrec
  final def drop(n: Int): List[A] = this match {
    case Nil => Nil
    case Cons(x,xs) if n == 1 => xs 
    case Cons(x,xs) => xs.drop(n-1)
  }
    

  @annotation.tailrec
  final def dropWhile(f: A => Boolean): List[A] = this match {
    case Nil => Nil
    case Cons(x,xs) if f(x) => xs.dropWhile(f)
    case _ => this
  }

  final def append[B >: A](a2: List[B]): List[B] = this match {
    case Nil          => a2
    case Cons(hd, tl) => Cons(hd, tl.append(a2))
  }

  final def init(): List[A] = this match {
      case Nil => Nil
      case Cons(x,Nil) => Nil
      case Cons(x,xs) => Cons(x, xs.init)
    }
    
  

  final def setHead[B >: A](head: B): List[B] = this match {
    case Nil => sys.error("setHead on empty list")
    case Cons(x,xs) => Cons(head, xs)
  } 

  //not tail rec
  final def foldRight[B](z: B)(f: (A, B) => B): B = this match {
    case Nil          => z
    case Cons(hd, tl) => f(hd, tl.foldRight(z)(f))
  }

  final def length(): Int = 
    this.foldRight(0)((a,b) => b + 1)
  
  final def length2(): Int = ???
  

  @annotation.tailrec
  final def foldLeft[B](z: B)(implicit f: (B, A) => B): B = this match {
    case Nil => z
    case Cons(x,xs) => xs.foldLeft(f(z,x))
  }
    

  final def reverse(): List[A] = 
    this.foldLeft(List[A]())((b,a) => Cons(a,b))

  final def foldRightInTermOfFoldLeft[B](z: B)(f: (A, B) => B): B =    
    this.reverse.foldLeft(z)((b,a) => f(a,b))
  
   
  final def foldLeftInTermOfFoldRight[B](z: B)(f: (B, A) => B): B = 
    this.reverse.foldRight(z)((b,a) => f(a,b))

  final def appendInTermsOfFoldLeft[B >: A](bl: List[B]): List[B] = 
    this.reverse.foldLeft(bl)((b,a) => Cons(a,b))

  final def map[B](f: A => B): List[B] = this match {
    case Nil => Nil
    case Cons(x,xs) => Cons(f(x), xs.map(f))
  }
    
  final def mapInTermsOfFoldLeft[B](f: A => B): List[B] = 
    this.reverse.foldLeft(List[B]())((acc,v) => Cons(f(v), acc))

  final def filter(f: A => Boolean): List[A] = this match {
    case Nil => Nil
    case Cons(x,xs) if f(x) => Cons(x, xs.filter(f))
    case Cons(x,xs) => xs.filter(f)
  }
  
  final def filterInTermsOfFoldLeft(f: A => Boolean): List[A] =
    this.reverse.foldLeft(List[A]())((acc,v) => {
      if(f(v)) Cons(v,acc)
      else acc 
    })
    

  final def flatMap[B](f: A => List[B]): List[B] = {
    val mapped = this.mapInTermsOfFoldLeft(f)
    List.concat(mapped)
  }
    

  final def filterInTermsOfFlatMap(f: A => Boolean): List[A] =
    this.flatMap( a => {
      if(f(a)){
        List(a)
      }else {
        Nil
      }
    })
    
  final def zipWith[B, C](ls: List[B])(implicit f: (A,B) => C): List[C] = (this,ls) match {
    case (Nil,_) => Nil
    case (_,Nil) => Nil
    case (Cons(x,xs), Cons(y,ys)) => Cons(f(x,y), xs.zipWith(ys))
  }  
  
  final def startsWith[B >: A](sub: List[B]): Boolean = (this,sub) match {
    case (_,Nil) => true  
    case (Nil,_) => false  
    case (Cons(x,_), Cons(y,_)) if(x!=y)=> false         
    case (Cons(x,xs), Cons(y,ys)) if (x == y) => xs.startsWith(ys)  
  }
  
  
  final def hasSubsequence[B >: A](sub: List[B]): Boolean = (this,sub) match {
    case (Nil, Cons(y,ys)) => false
    case _ if this.startsWith(sub) => true  
    case (Cons(x,xs),_) => xs.hasSubsequence(sub) 
  }
  
  
 

}

case object Nil extends List[Nothing]

case class Cons[+A](head: A, theTail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def sum(ints: List[Int]): Int = ints match {
    case Nil          => 0
    case Cons(hd, tl) => hd + sum(tl)
  }

  def product(ints: List[Int]): Int = ints match {
    case Nil          => 0
    case Cons(hd, tl) => hd * sum(tl)
  }

  def product2(ints: List[Int]): Int = 
    ints.foldRight(1)(_ * _)

  def sum2(ints: List[Int]): Int = 
    ints.foldRight(1)(_ + _)

  def product3(ints: List[Int]): Int =
    ints.foldLeft(1)(_ * _)

  def concat[A](l: List[List[A]]): List[A] = 
    l.foldLeft(List[A]())((acc,a) => acc.append(a))

}