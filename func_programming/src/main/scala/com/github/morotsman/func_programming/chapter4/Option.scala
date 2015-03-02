package com.github.morotsman.func_programming.chapter4

sealed trait Option[+A]{
    
  def map[B](f: A =>B): Option[B] = this match {
    case None => None
    case Some(v) => Some(f(v))
  }
   
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(v) => v
  }
  
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(v) => f(v)
  }
  
  //def flatMap2[B](f: A => Option[B]): Option[B] =
    
  
  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case Some(_) => this
  }
  
  def filter(f: A => Boolean): Option[A] = this match {
    case None => None
    case Some(v) if f(v) => this
    case _ => None
  }

    
    
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]


object Option {
  

  
  

}