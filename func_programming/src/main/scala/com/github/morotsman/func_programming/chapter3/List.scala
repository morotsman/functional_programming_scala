package com.github.morotsman.func_programming.chapter3

sealed trait List[+A] {

  //Exercise 3.2
  def tail(): List[A] = this match {
    case Nil => Nil
    case Cons(x,xs) => xs
  }

  //Exercise 3.4
  @annotation.tailrec
  final def drop(n: Int): List[A] = (n,this) match {
    case (_,Nil) => Nil
    case (0,_) => this
    case (_,Cons(x,xs)) => xs.drop(n-1)
  }
    
  //Exercise 3.5
  @annotation.tailrec
  final def dropWhile(f: A => Boolean): List[A] = this match {
    case Cons(x,xs) if f(x) => xs.dropWhile(f)
    case _ => this
  }

  final def append[B >: A](a2: List[B]): List[B] = this match {
    case Nil => a2
    case Cons(x,xs) => Cons(x,xs.append(a2))
  }

  //Exercise 3.6
  final def init(): List[A] = this match {
    case Nil => Nil
    case Cons(x1,Nil) => Nil
    case Cons(x,xs) => Cons(x,xs.init())
  }
    
  //Exercise 3.3
  final def setHead[B >: A](head: B): List[B] = this match {
    case Nil => Nil
    case Cons(x,xs) => Cons(head,xs)
  }

  //not tail rec
  final def foldRight[B](z: B)(f: (A, B) => B): B = this match {
    case Nil => z
    case Cons(x,xs) => f(x, xs.foldRight(z)(f))
  }

  //Exercise 3.9
  final def length(): Int = this.foldRight(0)((a,b) => b+1)
  
  final def length2(): Int = ???
  
  //Exercise 3.10
  final def foldLeft[B](z: B)(implicit f: (B, A) => B): B = {
    @annotation.tailrec
    def go(l: List[A], acc: B): B = l match {
      case Nil => acc
      case Cons(x,xs) => go(xs, f(acc,x))
    }
    
    go(this,z)   
  }
   
  //Exercise 3.12
  final def reverse(): List[A] = this.foldLeft(Nil:List[A])((acc,v) => Cons(v,acc))
  
  //Exercise 3.13
  final def foldLeftInTermOfFoldRight[B](z: B)(f: (B, A) => B): B = 
    this.reverse.foldRight(z)((a,b) => f(b,a))

  //Exercise 3.13
  final def foldRightInTermOfFoldLeft[B](z: B)(f: (A, B) => B): B = 
    this.reverse.foldLeft(z)((a,b) => f(b,a))
    
  //Exercise 3.14  
  final def appendInTermsOfFoldLeft[B >: A](bl: List[B]): List[B] = 
    this.reverse.foldLeft(bl)((acc,a) => Cons(a,acc))

  //Exercise 3.18
  final def map[B](f: A => B): List[B] = this match {
    case Nil => Nil
    case Cons(x,xs) => Cons(f(x), xs.map(f))
  }
    
  final def mapInTermsOfFoldLeft[B](f: A => B): List[B] = 
    this.reverse.foldLeft(Nil: List[B])((acc,a) => Cons(f(a), acc))

  //Exercise 3.19
  final def filter(f: A => Boolean): List[A] = this match {
    case Nil => Nil
    case Cons(x,xs) if f(x) => Cons(x,xs.filter(f))
    case Cons(_,xs) => xs.filter(f)
  }
  
  final def filterInTermsOfFoldLeft(f: A => Boolean): List[A] = 
    this.reverse.foldLeft(Nil: List[A])((acc, a) => if (f(a)) Cons(a, acc) else acc)
    
  //Exercise 3.20
  final def flatMap[B](f: A => List[B]): List[B] = this match {
    case Nil => Nil
    case Cons(x,xs) => f(x).append(xs.flatMap(f))   
  }
    
  final def filterInTermsOfFlatMap(f: A => Boolean): List[A] = 
    this.flatMap(a => if(f(a)) List(a) else Nil)
    
  //Exercise 3.23  
  final def zipWith[B, C](ls: List[B])(implicit f: (A,B) => C): List[C] = (this, ls) match {
    case (Nil, _) => Nil
    case (_,Nil) => Nil
    case (Cons(a,as), Cons(b,bs)) => Cons(f(a,b), as.zipWith(bs))
  }
  
  final def forAll(f: A => Boolean) : Boolean = 
    this.foldLeft(true)(_ && f(_))
    
  final def startsWith[B >: A](sub: List[B]): Boolean = (this,sub) match {
    case (_, Nil) => true
    case (Nil, _) => false
    case (Cons(a,as), Cons(b, bs)) if(a==b) => as.startsWith(bs)
    case _ => false
  }
  
   //Exercise 3.24
  final def hasSubsequence[B >: A](sub: List[B]): Boolean = ???
    
  

}

case object Nil extends List[Nothing]

case class Cons[+A](head: A, theTail: List[A]) extends List[A]

object List {

  //given by the book
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  //example from the book
  def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x,xs) => x + sum(xs) 
    }

  //example from the book
  def product(ints: List[Int]): Int = ints match {
    case Nil => 1
    case Cons(0,xs) => 0
    case Cons(x,xs) => x * product(xs)
  }

  //Exercise 3.7
  def product2(ints: List[Int]): Int = ints.foldRight(1)(_*_)

  //Exercise 3.11
  def sum2(ints: List[Int]): Int = ???

  //Exercise 3.11
  def product3(ints: List[Int]): Int = ???

  //Exercise 3.15
  def concat[A](l: List[List[A]]): List[A] = 
    l.foldLeft(Nil:List[A])((acc,a) => acc.append(a))
    

}