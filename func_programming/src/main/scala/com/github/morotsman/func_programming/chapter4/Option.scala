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
  
  def flatMap2[B](f: A => Option[B]): Option[B] =
    this.map(f).getOrElse(None)
    
  
  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case Some(_) => this
  }
  
  def orElse2[B >: A](ob: => Option[B]): Option[B] =
    this.map(Some(_)).getOrElse(ob)
  
  def filter(f: A => Boolean): Option[A] = this match {
    case Some(v) if f(v) => this
    case _ => None
  }
  
  def filter2(f: A => Boolean): Option[A] =
    this.map(v => if(f(v)) Some(v) else None).getOrElse(None)
    
  def filter3(f: A => Boolean): Option[A] =
    this.flatMap(v => if(f(v)) Some(v) else None)   
     
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]


object Option {
  
  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f
  
  def Try[A](a: => A): Option[A] = 
    try Some(a)
    catch { case e : Exception => None}
  
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = {
    a.flatMap(v1 => b.map(v2 => f(v1,v2)))
  }
  
  def sequence[A](a: List[Option[A]]): Option[List[A]] = 
    a.foldRight(Some(Nil: List[A]): Option[List[A]])((v,acc) => acc.flatMap(acc1 => v.map(v1 => v1::acc1)))
    
  def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = 
    a.foldRight(Some(Nil: List[B]): Option[List[B]])((v,acc) => acc.flatMap(acc1 => f(v).map(v1 => v1::acc1)))
    
  

}