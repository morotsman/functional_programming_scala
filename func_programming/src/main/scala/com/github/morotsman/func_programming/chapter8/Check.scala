package com.github.morotsman.func_programming.chapter8
/*
import Prop._
import com.github.morotsman.func_programming.chapter6._
import com.github.morotsman.func_programming.chapter5.Stream
 
object Prop {
 
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int

  case object Passed extends Result {
    def isFalsified = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    def isFalsified = true
  }
  
  case object Proved extends Result {
    def isFalsified = false
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
      }.find(_.isFalsified).getOrElse(Passed)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g.forSize(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n - 1) / max + 1
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, n, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => {
      val (a, rng1) = g.sample.run(rng)
      Some(a, rng1)
    })

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace: \n ${e.getStackTrace.mkString("\n")}"

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = SimpleRNG(System.currentTimeMillis())): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) => println(s"! Falsified after $n passed tests:\n $msg")
      case Passed            => println(s"+ OK, passed $testCases tests.")
      case Proved => 
        println(s"+ OK, proved property.")
    }
  
  
  def check(p: => Boolean): Prop = Prop { (_,_,_) =>
    if(p) Proved else Falsified("()", 0)
  }

}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

  def &&(p: Prop): Prop = Prop {
    (max, n, rng) =>
      {
        val result1 = this.run(max, n, rng)
        if (result1 == Passed) {
          p.run(max, n, rng)
        } else {
          result1
        }
      }
  }

  def ||(p: Prop): Prop = Prop {
    (max, n, rng) =>
      {
        val result1 = this.run(max, n, rng)
        if (result1 == Passed) {
          result1
        } else {
          p.run(max, n, rng)
        }
      }
  }

}

sealed trait Result {
  def isFalsified: Boolean
}

object Gen {

  type Weight = Double

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State {
      RNG.nonNegativeLessThan(stopExclusive - start)
    }.map { randomValue => start + randomValue })

  def unit[A](a: => A): Gen[A] = Gen(State {
    RNG.unit(a)
  })

  def boolean: Gen[Boolean] = Gen(State {
    RNG.nonNegativeLessThan(2)
  }.map(v => if (v == 1) true else false))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap { v => if (v) g1 else g2 }

  def weighted[A](g1: (Gen[A], Weight), g2: (Gen[A], Weight)): Gen[A] =
    choose(0, 100).flatMap(v => if (v < g1._2 * 100) g1._1 else g2._1)

}

case class Gen[A](sample: State[RNG, A]) {

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(State {
    rng =>
      {
        val (a, rng2) = this.sample.run(rng)
        val mapped = f(a)
        mapped.sample.run(rng2)
      }
  })

  def map[B](f: A => B): Gen[B] =
    this.flatMap { a => Gen.unit(f(a)) }

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(numberOfElements => Gen.listOfN(numberOfElements, this))

  def unsized: SGen[A] = SGen {
    i => this
  }

}

object SGen {

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen {
    i =>
      {
        Gen.listOfN(i, g)
      }
  }

  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen {
    i => {
      Gen.listOfN(i+1,g)
    }
  }
}

case class SGen[A](forSize: Int => Gen[A]) {

  def flatMap[B](f: A => SGen[B]): SGen[B] = SGen {
    i =>
      {
        forSize(i).flatMap { a => f(a).forSize(i) }
      }
  }

  def map[B](f: A => B): SGen[B] = SGen {
    i =>
      {
        forSize(i).map { a => f(a) }
      }
  }
 

  def filter(f: A => Boolean): SGen[A] = ???

}

*/

