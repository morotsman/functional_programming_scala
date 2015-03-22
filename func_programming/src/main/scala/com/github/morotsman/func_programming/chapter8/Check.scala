package com.github.morotsman.func_programming.chapter8

import Prop._
import com.github.morotsman.func_programming.chapter6._

trait Prop {

  def check: Either[(FailedCase, SuccessCount), SuccessCount]

  /*answer to 8.3 before refactory of check
  def &&(p: Prop): Prop = {
    val that = this;
    new Prop{
      def check = p.check && that.check
    }    
  }
  */

}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
}

//State:  S => (A, S)
case class Gen[+A](sample: State[RNG, A]) {

}

object Gen {

  def listOf[A](a: Gen[A]): Gen[List[A]] = ???

  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] = ???

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen {
      State {
        (rng: RNG) =>
          val scale = Int.MaxValue / (stopExclusive - start)
          RNG.map(RNG.nonNegativeInt)(a => start + a / scale)(rng)
      }
    }

  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  def boolean: Gen[Boolean] =
    Gen {
      choose(0, 2).sample.map(v => if (v == 0) true else false)
    }

}