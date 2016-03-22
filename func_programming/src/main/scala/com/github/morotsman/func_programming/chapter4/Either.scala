package com.github.morotsman.func_programming.chapter4

sealed trait Either[+E, +A] {
  
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(v) => Left(v)
    case Right(v) => Right(f(v))
  }
    
  
  def flatMap[EE >: E, B](f: A => Either[EE,B]): Either[EE,B] = this match {
    case Left(v) => Left(v)
    case Right(v) => f(v)
  }  
    
  
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(v) => b
    case Right(v) => this
  }

  
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = 
    this.flatMap(aa => b.map(bb => f(aa,bb)))
    
    
  
}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Either{
  
  def Try[A](a: => A): Either[Exception, A] = 
    try Right(a)
    catch { case e: Exception => Left(e)}
  
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = 
    //es.reverse.foldLeft(Right(List[A]()):Either[E,List[A]])((a, v) => v.flatMap(vv => a.map(av => vv::av)))
    es.reverse.foldLeft(Right(List[A]()):Either[E,List[A]])((a, v) => v.map2(a)(_ :: _))
    
  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = 
    as.reverse.foldLeft(Right(List[B]()):Either[E,List[B]])((a,v) => f(v).map2(a)(_::_))
  
}