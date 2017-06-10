package com.github.morotsman.func_programming.chapter8

import com.github.morotsman.func_programming.chapter5.Stream
import com.github.morotsman.func_programming.chapter6.State
import com.github.morotsman.func_programming.chapter6.RNG
import com.github.morotsman.func_programming.chapter6.State._
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
  
  def boolean: Gen[Boolean] = Gen {
    State {
      RNG.boolean
    }
  }
  
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen {
    State{
      rng => ???
    }
  }

}

//trait Gen[A] {
//  def map[A,B](f: A => B): Gen[B] = ???
//  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
//}

case class Gen[A](sample: State[RNG, A])

trait SGen[+A] {

}


