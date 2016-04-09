package com.github.morotsman.func_programming.chapter6

trait RNG {
  def nextInt: (Int, RNG)

}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

}

object RNG {

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val next = rng.nextInt
    if (next._1 >= 0) {
      next
    } else {
      nonNegativeInt(next._2)
    }

  }

  def double(rng: RNG): (Double, RNG) = {
    val next = nonNegativeInt(rng)
    (next._1 / (1.0 + Int.MaxValue), next._2)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val nextInt = rng.nextInt
    val nextDouble = double(nextInt._2)
    ((nextInt._1, nextDouble._1), nextDouble._2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val nextDouble = double(rng)
    val nextInt = nextDouble._2.nextInt
    ((nextDouble._1, nextInt._1), nextInt._2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val double1 = double(rng)
    val double2 = double(double1._2)
    val double3 = double(double2._2)
    ((double1._1, double2._1, double3._1), double3._2)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {

    @annotation.tailrec
    def go(count: Int, rng: RNG, acc: List[Int]): (List[Int], RNG) = {
      if (count == 0) {
        (acc, rng)
      } else {
        val next = rng.nextInt
        go(count - 1, next._2, next._1 :: acc)
      }
    }

    go(count, rng, List())
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def doubleInTermsOfMap: Rand[Double] =
    map(nonNegativeInt)(v => v / (Int.MaxValue + 1))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((a, b) => (a, b))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldLeft((a: RNG) => (List(): List[A], a))((acc, a) => map2(acc, a)((a, b) => b :: a))

  def intsInTermsOfSequence(count: Int): Rand[List[Int]] = {
    sequence(List.fill(count)(1).map { i => int })
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (res, rng1) = map(f)(a => g(a))(rng)
      res(rng1)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt)(i => {
      val mod = i % n
      if (i + (n - 1) - mod >= 0) {
        unit(mod)
      } else {
        nonNegativeLessThan(n)
      }
    })

  def mapInTermsOfFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2InTermsOfFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => flatMap(rb)(b => rng => (f(a, b), rng)))
  }

}

import State._

case class State[S, +A](run: S => (A, S)) {

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s1 => {
      val (a, s2) = this.run(s1)
      f(a).run(s2)
    })

  def map[B](f: A => B): State[S, B] = {
    this.flatMap(v => unit(f(v)))
  }

}

object State {
  type Rand[A] = State[RNG, A]

  def sequence[A, S](ls: List[State[S, A]]): State[S, List[A]] =
    ls.foldRight(State((s: S) => (List(): List[A], s)))((acc, a) => a.flatMap(aa => acc.map(bb => bb :: aa)))

  def unit[A, S](a: A): State[S, A] =
    State(s => (a, s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

//The state
case class Machine(looked: Boolean, candies: Int, coins: Int)

// machine => ((coins, candies), Machine)
object Machine {

  def stateChange(prevState: State[Machine, (Int, Int)], a: Input): State[Machine, (Int, Int)] =
    prevState.flatMap(v =>
      State { m =>
        {
          if (m.candies == 0) {
            prevState.run(m)
          } else if (a == Coin && m.looked) {
            ((m.coins + 1, m.candies), Machine(false, m.candies, m.coins + 1))
          } else if (a == Turn && !m.looked) {
            ((m.coins, m.candies - 1), Machine(true, m.candies - 1, m.coins))
          } else {
            prevState.run(m)
          }
        }
      })

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    State(m => {
      val result =
        inputs
          .foldLeft(State((s: Machine) => ((s.coins, s.candies), s)))(stateChange)
      result.run(m)
    })
    
    /*
  def simulateMachine2(inputs: List[Input]): State[Machine, (Int, Int)] = {
    State{ machine =>
      val listOfStateChanges = inputs.map(input =>
        State { (m:Machine) => (
          input match {
            case _ if m.candies == 0 =>  ((m.coins, m.candies    ), Machine(m.looked, m.candies    , m.coins))
            case Coin if m.looked    =>  ((m.coins + 1, m.candies), Machine(false, m.candies, m.coins + 1))
            case Coin if !m.looked   =>  ((m.coins    , m.candies), Machine(false, m.candies, m.coins    ))
            case Turn if !m.looked   =>  ((m.coins, m.candies - 1), Machine(true, m.candies - 1, m.coins)) 
            case Turn if m.looked    =>  ((m.coins, m.candies    ), Machine(true, m.candies    , m.coins))          
          })
        })

      val (outputs, finalMachine) = sequence(listOfStateChanges).run(machine)
      println ("outputs: " + outputs)
      (outputs.head, finalMachine)
    } 
  }    
  
  */

}
