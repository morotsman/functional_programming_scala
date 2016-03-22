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
  
  
  def flatMap[B](f: A => Option[B]): Option[B] = 
    this.map(v => f(v)).getOrElse(None)
    
    
  
  def orElse[B >: A](ob: => Option[B]): Option[B] = 
    this.map(v => Some(v)).getOrElse(ob)
    
  
  def filter(f: A => Boolean): Option[A] = 
    this.flatMap(v => {
      if(f(v)){
        Some(v)
      }else {
        None
      }
    })
  
  
     
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]


object Option {
  
  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f
  
  def Try[A](a: => A): Option[A] = 
    try Some(a)
    catch { case e : Exception => None}
  
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = 
    a.flatMap(av => b.map(bv => f(av,bv)))
  
  def sequence[A](a: List[Option[A]]): Option[List[A]] = 
    //a.foldRight(Some(List()): Option[List[A]])((b,acc) => acc.flatMap(av => b.map(bv => bv::av)))
    a.foldRight(Some(List()): Option[List[A]])((b,acc) => map2(b,acc)(_::_))
    
  def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = 
    //a.foldRight(Some(List()): Option[List[B]])((b,acc) => acc.flatMap(av => f(b).map(bv => bv::av)))
    a.foldRight(Some(List()): Option[List[B]])((b,acc) => map2(f(b),acc)(_::_))
    
  def sequenceInTermsOfTraverse[A](a: List[Option[A]]): Option[List[A]] =  
    traverse(a)(v => v)
    
  

}