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

  def map[B](f: A => B): Gen[B] = Gen {
    this.sample.map(f)
  }

  def map2[B, C](gb: Gen[B])(f: (A, B) => C): Gen[C] = Gen {
    this.sample.flatMap { a => gb.sample.map { b => f(a, b) } }
  }

  def genOption(): Gen[Option[A]] =
    this.map(a => if (a == null) None else Some(a))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen {
      this.sample.flatMap(a => f(a).sample)
    }

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap { s => 
      Gen {
        State.sequence(List.fill(s)(this.sample))
      }
    }
  

    

}

object Gen {

  def listOf[A](a: Gen[A]): Gen[List[A]] = ???

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

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen {
    State.sequence(List.fill(n)(g.sample))
  }

  def chooseAPair(start: Int, stopExclusive: Int): Gen[(Int, Int)] =
    Gen {
      State {
        rng =>
          {
            val (result, rng2) = choose(start, stopExclusive).sample.run(rng)
            val (result2, rng3) = choose(start, stopExclusive).sample.run(rng)
            ((result, result2), rng3)
          }
      }
    }

  def chooseAPairInTermsOfMap2(start: Int, stopExclusive: Int): Gen[(Int, Int)] =
    choose(start, stopExclusive).map2(choose(start, stopExclusive))((a, b) => (a, b))
    
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = 
    choose(0,2).flatMap(v => if(v == 0) g1 else g2)
    
  def weighted[A](g1: (Gen[A], Int), g2: (Gen[A], Int)): Gen[A] = {
    val limit = g1._2
    choose(0, g1._2 + g2._2).flatMap { v => if(v<limit) g1._1 else g2._1 }
  }
    

}