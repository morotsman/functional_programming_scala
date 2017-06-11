package com.github.morotsman.func_programming.chapter8

import com.github.morotsman.func_programming.chapter5.Stream
import com.github.morotsman.func_programming.chapter6.State
import com.github.morotsman.func_programming.chapter6.RNG
import Gen._
import Prop._
import java.util.concurrent.{ Executors, ExecutorService }

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop {

  def check: Either[(FailedCases, SuccessCount), SuccessCount] = ???

  //def &&(p: Prop): Prop = new Prop {
  //  override def check = Prop.this.check && p.check
  //}
}

object Prop {
  type FailedCases = String
  type SuccessCount = Int

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen {
    State {
      RNG.unit(a)
    }
  }
  

  def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen {
    State {
      RNG.between(start, stopExclusive)
    }
  }
  
  def boolean: Gen[Boolean] = 
    choose(0,2).map((i:Int) => if(i == 0) true else false)
  
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = 
    List.fill(n)(g).foldRight(unit(Nil: List[A]))((ga,gbs) => gbs.flatMap(bs => ga.map(a => a::bs)))
    
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = 
    for {
      a1 <- g1;
      a2 <- g2;
      b <- boolean
    } yield(if(b) a1 else a2)
    

}

//trait Gen[A] {
//  def map[A,B](f: A => B): Gen[B] = ???
//  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
//}

case class Gen[A](sample: State[RNG, A]){
  
  def map[B](f: A => B): Gen[B] = 
   flatMap(a => unit(f(a)))
  
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen {
    sample.flatMap(f(_).sample)
  }
  
  def listOfN(size: Gen[Int]): Gen[List[A]] = 
    size.flatMap(s => Gen.listOfN(s,this))
  
}

trait SGen[+A] {

}


