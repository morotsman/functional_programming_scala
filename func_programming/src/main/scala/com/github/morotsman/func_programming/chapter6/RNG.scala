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
  
  type Rand[+A] = RNG => (A, RNG)

  def randomPair(rng: RNG): ((Int,Int), RNG) = {
    val (i1,rng2) = rng.nextInt
    val (i2,rng3) = rng2.nextInt
    ((i1,i2), rng3)
  }
  
  def between(start: Int, stopExclusive: Int): Rand[Int] = rng => {
    val (i, rng2) = nonNegativeInt(rng)
    val value = start + i % (stopExclusive-start)
    (value,rng2)
  }
  
  def boolean: Rand[Boolean] = { rng => 
    val (i,rng2) = rng.nextInt
    if(i % 2 == 0) (true,rng2) else (false,rng2)
  }
  
  
  def nonNegativeInt: Rand[Int] = rng => {
    val (i, rng2) = rng.nextInt
    if(i > -1) (i,rng2) else nonNegativeInt(rng2)
  }
  
  def double(rng: RNG): (Double, RNG) = {
    val (i, rng2) = nonNegativeInt(rng)
    (i.toDouble/Int.MaxValue.toDouble, rng2)
  }
  
  def intDouble(rng: RNG): ((Int,Double),RNG) = {
    val (i,rng2) = rng.nextInt
    val (d,rng3) = double(rng2)
    ((i,d),rng3)
  }
  
  def doubleInt(rng: RNG): ((Double,Int),RNG) = {
    val ((i,d), rng3) = intDouble(rng)
    ((d,i),rng3)
  }  
  
  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1,rng2) = double(rng)
    val (d2,rng3) = double(rng2)
    val (d3,rng4) = double(rng3)
    ((d1,d2,d3), rng4)
  }
  
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if(count == 0) (Nil,rng)
    else {
      val (randomInts, rng2) = ints(count-1)(rng)
      val (i,rng3) = rng2.nextInt
      (i::randomInts, rng3)
    }
  }
  
  val int : Rand[Int] = _.nextInt
  
  def unit[A](a: A): Rand[A] = 
    rng => (a,rng)
    
  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = 
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
    
  def nonNegativeEven: Rand[Int] = 
    map(nonNegativeInt)(i => i - i % 2)
    
  def doubleInTermsOfMap: Rand[Double] =
    map(nonNegativeInt)(i => i.toDouble/Int.MaxValue.toDouble)
    
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = rng => {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a,b), rng3)
  }
  
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = 
    fs.foldLeft(unit(List[A]()))(map2(_,_)(((la,a) => a::la)))
    
  def intsInTermsOfSequence(count: Int): Rand[List[Int]] = 
    sequence(List.fill(count)(1).map(v => int))
    
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a,rng1) = f(rng)
    g(a)(rng1)
  }
    
  
  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt)(i => {
      if(i < n) unit(i)
      else nonNegativeLessThan(n)        
    })
    
  def mapInTermsOfFlatMap[A,B](ra: Rand[A])(f: A => B): Rand[B] = 
    flatMap(ra)(a => unit(f(a)))
    
  def map2InTermsOfFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = 
    flatMap(ra)(a => map(rb)(b => f(a,b)))

}

import State._

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))
    
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a,b)))
    
  def flatMap[B](f: A => State[S, B]): State[S, B] = State (s =>{
    val (a,s2) = run(s)
    f(a).run(s2)
  })
  
  def get[S]: State[S,S] = State(s => (s,s))
  
  def set[S](s : S) : State[S,Unit] = State(_ => ((), s)) 
  
  def modify[S](f: S => S): State[S,Unit] = for{
    s <- get
    _ <- set(f(s)) 
  } yield ()
    
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  
  def unit[S,A](a: A): State[S,A] = State(s => (a,s))
  
  def sequence[S,A](ls: List[State[S,A]]) : State[S, List[A]] = 
    ls.foldRight(unit[S,List[A]](Nil))((sla,sb) => sla.map2(sb)((a,la) => a::la))
  
  type Rand[A] = State[RNG, A]
  
  def applyMachineRules(m: Machine, input: Input): Machine = 
      if (input == Coin && m.locked && m.candies > 0) Machine(false, m.candies, m.coins + 1)
      else if (input == Turn && !m.locked && m.candies > 0) Machine(true, m.candies - 1, m.coins)
      else m
  
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State ( m => {
    val finalMachine = inputs.foldLeft(m)(applyMachineRules)
    
    ((finalMachine.coins,finalMachine.candies), finalMachine) 
  })
   
    
    
    
}


