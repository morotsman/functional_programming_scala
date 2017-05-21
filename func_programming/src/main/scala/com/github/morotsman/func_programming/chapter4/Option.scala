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
 
  /*
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(v) => f(v)
  }
  */
  
  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)
    
  /*
  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case Some(v) => Some(v)
  }
  */
  
  def orElse[B >: A](ob: => Option[B]): Option[B] = map(a => Some(a)).getOrElse(ob)
    
  /*
  def filter(f: A => Boolean): Option[A] = this match {
    case None => None
    case Some(v) => if(f(v)) Some(v) else None
  }
  */
  
  def filter(f: A => Boolean): Option[A] = 
    map(a => if(f(a)) Some(a) else None: Option[A]).getOrElse(None)
  
     
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]


object Option {
  
  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f
  
  def Try[A](a: => A): Option[A] = 
    try Some(a)
    catch { case e : Exception => None}
  
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = 
    for(
      av <- a;
      bv <- b
    ) yield f(av,bv)
  
  def sequence[A](as: List[Option[A]]): Option[List[A]] = 
    as.foldRight(Some(List()): Option[List[A]])(map2(_,_)(_::_))
    
  def traverse[A,B](as: List[A])(f: A => Option[B]): Option[List[B]] = 
    as.foldRight(Some(List()): Option[List[B]])((a,b) =>  map2(f(a),b)(_::_))
    
  def sequenceInTermsOfTraverse[A](a: List[Option[A]]): Option[List[A]] =  
    traverse(a)(identity)

}