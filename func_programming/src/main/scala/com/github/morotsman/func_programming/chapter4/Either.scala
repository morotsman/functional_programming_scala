package com.github.morotsman.func_programming.chapter4

sealed trait Either[+E, +A] {
  
  def map[B](f: A => B): Either[E, B] = 
    flatMap(a => Right(f(a)))
    
  def flatMap[EE >: E, B](f: A => Either[EE,B]): Either[EE,B] = this match {
    case Left(l) => Left(l)
    case Right(r) => f(r)
  }
      
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(l) => b
    case Right(r) => this
  }
  
  def map2[EE >: E, B, C](bs: Either[EE, B])(f: (A, B) => C): Either[EE, C] = 
    flatMap(a => bs.map(b => f(a,b)))

}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Either{
  
  def Try[A](a: => A): Either[Exception, A] = 
    try Right(a)
    catch { case e: Exception => Left(e)}
  
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = 
    traverse(es)(identity)
    
  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = 
    as.foldRight(Right(List()): Either[E,List[B]])((a,b) => f(a).map2(b)(_::_))

  
}