package com.github.morotsman.func_programming.chapter8

import Prop._
import com.github.morotsman.func_programming.chapter6._
import com.github.morotsman.func_programming.chapter5.Stream

object Prop {
  type MaxSize = Int
  type FailedCase = List[String]
  type SuccessCount = Int
  type TestCases = Int

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified = false
  }

  case class Falsified(failure: FailedCase, success: SuccessCount) extends Result {
    def isFalsified = true
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      randomStream(as)(rng).zip(Stream.from(0)).take(n).map({
         case (a, i) =>
          try {
            if (f(a)) Passed else Falsified(List(a.toString()), i)
          } catch { case e: Exception => Falsified(List(buildMsg(a, e)), i) }       
      }).find(a => a.isFalsified).getOrElse(Passed)  
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
      
      
  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(n => g.forSize(n))(f)

    
  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max,n,rng) =>
      val casesPerSize = (n - 1) / max + 1
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, n, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max,n,rng)
  } 


}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  
  def &&(p: Prop): Prop = Prop{
    (m, t, rng) => {
      val res1 = this.run(m, t,rng)
      res1 match {
        case(Falsified(_,_)) => res1
        case(_) => p.run(m, t,rng)
      }
    }
  }
  
  def ||(p: Prop): Prop = Prop {
    (m, t, rng) => {
      val res1 = this.run(m, t,rng)
      res1 match {
        case(Passed) => Passed
        case(Falsified(msg,_)) => Prop { 
          (m2, t2, rng2) => {
            val res2 = p.run(m2, t2,rng2)
            res2 match {
              case(Passed) => res2
              case(Falsified(msg2, a)) => Falsified(msg2 ++ msg, a) 
            }            
          }
        }.run(m, t,rng)
      }
    }  
  }
}

case class SGen[+A](forSize: Int => Gen[A]) {
  
  def apply(n: Int): Gen[A] = forSize(n)
  
  def map[B](f: A => B): SGen[B] = SGen {
    i => this.forSize(i).map(f)
  }
  
  def flatMap[B](f: A => Gen[B]): SGen[B] = SGen {
    i => {
      this.forSize(i).flatMap(f)
    }
  }
  
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
  
  def unsized: SGen[A] = SGen {
    i => this
  } 

}

object Gen {

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
  
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen {
    i => listOfN(i, g)
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
    choose(0, 2).flatMap(v => if (v == 0) g1 else g2)

  def weighted[A](g1: (Gen[A], Int), g2: (Gen[A], Int)): Gen[A] = {
    val limit = g1._2
    choose(0, g1._2 + g2._2).flatMap { v => if (v < limit) g1._1 else g2._1 }
  }
  
  

}