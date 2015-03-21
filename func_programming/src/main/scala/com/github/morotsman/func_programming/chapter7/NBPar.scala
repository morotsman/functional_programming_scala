package com.github.morotsman.func_programming.chapter7

import java.util.concurrent._
import java.util.concurrent.atomic.AtomicReference



object NBPar {
  
  sealed trait Future[+A] {
    private[chapter7] def apply(k: A => Unit): Unit
  }
  
  type NBPar[+A] = ExecutorService => Future[A]
  
  def run[A](es: ExecutorService)(p: NBPar[A]): A  ={
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    p(es) { a => ref.set(a); latch.countDown }
    latch.await
    ref.get
  }
  
  def unit[A](a: A): NBPar[A] = 
    es => new Future[A] {
      def apply(cb: A => Unit): Unit = cb(a)
    }
    
  def fork[A](a: => NBPar[A]): NBPar[A] = 
    es => new Future[A] {
      def apply(cb: A => Unit): Unit = 
        eval(es)(a(es)(cb))
    }
    
  def eval(es: ExecutorService)(r: => Unit): Unit = 
    es.submit(new Callable[Unit] {def call = r})
    
  def map2[A,B,C](p: NBPar[A], p2: NBPar[B])(f: (A,B) => C): NBPar[C] = 
    es => new Future[C]{
      def apply(cb: C => Unit): Unit = {
        var ar: Option[A] = None
        var br: Option[B] = None
        
        val combiner = Actor[Either[A,B]](es) {
          case Left(a) => br match {
            case None => ar = Some(a)
            case Some(b) => eval(es)(cb(f(a,b)))
          }
          
          case Right(b) => ar match {
            case None => br = Some(b)
            case Some(a) => eval(es)(cb(f(a,b)))
          }
          
        }
        p(es)(a => combiner ! Left(a))
        p2(es)(b => combiner ! Right(b))
      }
    }
    
    def lazyUnit[A](a: => A): NBPar[A] = fork(unit(a))
    
  def asyncF[A, B](f: A => B): A => NBPar[B] = 
    (a: A) => lazyUnit(f(a))
    
  def map[A, B](pa: NBPar[A])(f: A => B): NBPar[B] = 
    map2(pa, unit(()))((a,_) => f(a))
    
 
    
    def sequenceBalanced[A](as: IndexedSeq[NBPar[A]]): NBPar[IndexedSeq[A]] = fork {
      if (as.isEmpty) unit(Vector())
      else if (as.length == 1) map(as.head)(a => Vector(a))
      else {
        val (l,r) = as.splitAt(as.length/2)
        map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
      }
    }

    def sequence[A](as: List[NBPar[A]]): NBPar[List[A]] =
      map(sequenceBalanced(as.toIndexedSeq))(_.toList)
    
  def parMap[A,B](ps: List[A])(f: A => B): NBPar[List[B]] = fork {
      val fbs: List[NBPar[B]] = ps.map(asyncF(f))
      sequence(fbs)
    }
    
  def parFilter[A](as: List[A])(f: A => Boolean): NBPar[List[A]] = {
    val tmp = as.map(asyncF(x => if(f(x)) List(x) else Nil))
    map(sequence(tmp))(_.flatMap { identity})
  }
  
  def reduce[A](z: A)(s: IndexedSeq[A])(f: (A, A) => A): NBPar[A] = 
     if(s.size <= 1)
      unit(s.headOption.getOrElse(z))
    else {
      val (l, r) = s.splitAt(s.length/2)
      map2(reduce(z)(l)(f), reduce(z)(r)(f))(f(_,_))
    }   

}