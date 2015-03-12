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
  
}