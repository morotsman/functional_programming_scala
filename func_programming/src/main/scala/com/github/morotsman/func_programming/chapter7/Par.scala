package com.github.morotsman.func_programming.chapter7

import java.util.concurrent._

object Par{
  
  type Par[A] = ExecutorService => Future[A]
  
  def unit[A](a: => A): Par[A] = (es: ExecutorService) => UnitFuture(a)
  
  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }
  
  def map2[A,B,C](a: => Par[A], b: => Par[B])(f: (A, B) => C): Par[C] = 
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }
  
  def fork[A](a: => Par[A]): Par[A] = 
    es => es.submit(new Callable[A]{
      def call = a(es).get
    })
  
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
  
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)
  
  def asyncF[A, B](f: A => B): A => Par[B] = 
    (a: A) => lazyUnit(f(a))
    
  def map[A, B](pa: Par[A])(f: A => B): Par[B] = 
    map2(pa, unit(()))((a,_) => f(a))
    
  def sortPar(parList: Par[List[Int]]) : Par[List[Int]] = 
    map(parList)(_.sorted)
    
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = 
    ps.foldRight(unit(Nil: List[A]))((curr, acc) => map2(curr,acc)((v, l) => v::l))
    
  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
      val fbs: List[Par[B]] = ps.map(asyncF(f))
      sequence(fbs)
    }
    
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val tmp = as.map(asyncF(x => if(f(x)) List(x) else Nil))
    map(sequence(tmp))(_.flatMap { identity})
  }
  
  
  def sum(ints: IndexedSeq[Int]): Par[Int] = 
    if(ints.size <= 1)
      Par.unit(ints.headOption.getOrElse(0))
    else {
      val (l, r) = ints.splitAt(ints.length/2)
      Par.map2(sum(l), sum(r))(_ + _)
    }
  
  def reduce[A](z: A)(s: IndexedSeq[A])(f: (A, A) => A): Par[A] = 
     if(s.size <= 1)
      Par.unit(s.headOption.getOrElse(z))
    else {
      val (l, r) = s.splitAt(s.length/2)
      Par.map2(reduce(z)(l)(f), reduce(z)(r)(f))(f(_,_))
    }  
  
   
  
  
  def sum2(ints: IndexedSeq[Int]): Par[Int] =
    reduce(0)(ints)(_+_)
    
    
  def map3[A,B,C,D](a: => Par[A], b: => Par[B], c: => Par[C])(f: (A, B, C) => D): Par[D] = {
    map2(map2(a, b)((a,b) => (a,b)), c)((a_b, c) => f(a_b._1, a_b._2, c)) 
  }
  
  def map4[A,B,C,D, E](a: => Par[A], b: => Par[B], c: => Par[C], d: => Par[D])(f: (A, B, C, D) => E): Par[E] = {
    map2(map3(a, b, c)((a,b,c) => (a,b,c)), d)((a_b_c,d) => f(a_b_c._1, a_b_c._2, a_b_c._3, d)) 
  }  
    
    
    
  
}