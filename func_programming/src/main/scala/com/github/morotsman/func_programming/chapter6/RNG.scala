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
    val (res, rng2) = rng.nextInt
    val positiveRes = if (res < 0) Math.abs(res + 1) else res
    (positiveRes, rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (res, rng2) = nonNegativeInt(rng)
    (res / (Int.MaxValue.toDouble + 1), rng2)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (res, rng2) = rng.nextInt
    val (res2, rng3) = rng2.nextInt
    ((res, res2.toDouble), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (res, rng2) = rng.nextInt
    val (res2, rng3) = rng2.nextInt
    ((res.toDouble, res2), rng3)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (res, rng2) = rng.nextInt
    val (res2, rng3) = rng2.nextInt
    val (res3, rng4) = rng3.nextInt
    ((res.toDouble, res2.toDouble, res3.toDouble), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0) (Nil, rng)
    else {
      val (res, rng2) = rng.nextInt
      val (list, rng3) = ints(count - 1)(rng2)
      (res :: list, rng3)
    }
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
    map(nonNegativeInt)(res => res / (Int.MaxValue.toDouble + 1))

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
    fs.foldRight((rng: RNG) => (Nil: List[A], rng))((cur, acc) => map2(cur, acc)((v, a) => v :: a))

  def intsInTermsOfSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(rng => rng.nextInt))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (res, rng2) = f(rng)
      g(res)(rng2)
    }

  def mapInTermsOfFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => rng => (f(a), rng))

  def map2InTermsOfFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))
}

import State._

case class State[S, +A](run: S => (A, S)) {

  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    State(s => {
      val (a1, s1) = this.run(s)
      f(a1).run(s1)
    })
  }

  def map[B](f: A => B): State[S, B] =
    this.flatMap(a => unit(f(a)))

}

object State {
  type Rand[A] = State[RNG, A]

  def sequence[A, S](ls: List[State[S, A]]): State[S, List[A]] = {
    ls.reverse.foldRight(State((s: S) => {
      (Nil: List[A], s)
    }))((cur, acc) => acc.flatMap { l => cur.map { a => a :: l } })   
  }

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

// machine => ((Int, Int), Machine)
object Machine {


  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
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
      (outputs.head, finalMachine)
    } 
  }
  

}
