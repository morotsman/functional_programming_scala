package com.github.morotsman.func_programming.chapter3

sealed trait List[+A] {

  def tail(): List[A] = this match {
    case Nil          => Nil
    case Cons(hd, tl) => tl
  }

  @annotation.tailrec
  final def drop(n: Int): List[A] = this match {
    case Nil          => this
    case Cons(hd, tl) => tl.drop(n - 1)
  }

  @annotation.tailrec
  final def dropWhile(f: A => Boolean): List[A] = this match {
    case Nil                   => Nil
    case Cons(hd, tl) if f(hd) => tl.dropWhile(f);
    case _                     => this
  }

  def append[B >: A](a2: List[B]): List[B] = this match {
    case Nil          => a2
    case Cons(hd, tl) => Cons(hd, tl.append(a2))
  }

  def init(): List[A] = this match {
    case Nil           => Nil
    case Cons(hd, Nil) => Nil
    case Cons(hd, tl)  => Cons(hd, tl.init())
  }

  def setHead[B >: A](head: B): List[B] = this match {
    case Nil         => sys.error("not possible on empty list")
    case Cons(_, tl) => Cons(head, tl)
  }

  def foldRight[B](z: B)(f: (A, B) => B): B = this match {
    case Nil          => z
    case Cons(hd, tl) => f(hd, tl.foldRight(z)(f))
  }

  def length2(): Int = this.foldRight(0)((_, acc) => acc + 1)
  def length(): Int = this.foldLeft(0)((acc, _) => acc + 1)

  def foldLeft[B](z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def go(as: List[A], acc: B): B = as match {
      case Nil          => acc
      case Cons(hd, tl) => go(tl, f(acc, hd))
    }

    go(this, z)
  }

  def reverse(): List[A] =
    this.foldLeft(Nil: List[A])((acc, a) => Cons(a, acc))

  def foldRightInTermOfFoldLeft[B](z: B)(f: (A, B) => B): B =
    this.reverse.foldLeft(z)((a, acc) => f(acc, a))

  def foldLeftInTermOfFoldRight[B](z: B)(f: (B, A) => B): B =
    this.reverse.foldRight(z)((acc, a) => f(a, acc))

  def appendInTermsOfFoldLeft[B >: A](a2: List[B]): List[B] =
    this.reverse.foldLeft(a2)((acc, a) => Cons(a, acc))

  def map[B](f: A => B): List[B] =
    this.reverse.foldLeft(Nil: List[B])((acc, a) => Cons(f(a), acc))

  def filter(f: A => Boolean): List[A] = this match {
    case Nil                   => Nil
    case Cons(hd, tl) if f(hd) => Cons(hd, tl.filter(f))
    case Cons(_, tl)           => tl.filter(f)
  }

  def filter2(f: A => Boolean): List[A] = {
    @annotation.tailrec
    def go(as: List[A], acc: List[A]): List[A] = as match {
      case Nil                   => acc.reverse
      case Cons(hd, tl) if f(hd) => go(tl, Cons(hd, acc))
      case Cons(_, tl)           => go(tl, acc)
    }

    go(this, Nil)
  }

  def filter3(f: A => Boolean): List[A] =
    this.reverse.foldLeft(Nil: List[A])((acc, a) => if (f(a)) Cons(a, acc) else acc)

  def flatMap[B](f: A => List[B]): List[B] =
    this.reverse.foldLeft(Nil: List[B])((acc, a) => f(a).append(acc))

  def filterInTermsOfFlatMap(f: A => Boolean): List[A] =
    this.flatMap(a => if (f(a)) List(a) else Nil)
    
  def zipWith[B, C](ls: List[B])(implicit f: (A,B) => C): List[C] = (this, ls) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(hd1,tl1), Cons(hd2,tl2)) => Cons(f(hd1,hd2), tl1.zipWith(tl2))
  }
  
  def hasSubsequence[B >: A](sub: List[B]): Boolean = {
    
    if(this == Nil && sub == Nil) return true
    
    def startsWith(ls: List[A], sub: List[B]): Boolean = (ls, sub) match{
      case (Nil, Nil) => true
      case (Nil,_) => false
      case (_, Nil) => true
      case (Cons(hd1,tl1), Cons(hd2,tl2)) if hd1==hd2 => startsWith(tl1,tl2) 
      case _ => false
    }
    
    def loop(ls: List[A]): Boolean = ls match {
      case Nil => false
      case Cons(_, _) if startsWith(ls, sub) => true
      case Cons(_, tl) => loop(tl)
    }
    
    loop(this)
    
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
    ints.foldRight(0)(_ * _)

  def sum2(ints: List[Int]): Int =
    ints.foldLeft(0)(_ + _)

  def product3(ints: List[Int]): Int =
    ints.foldLeft(1)(_ * _)

  def concat[A](l: List[List[A]]): List[A] =
    l.foldLeft(Nil: List[A])((acc, a) => acc.append(a))

}