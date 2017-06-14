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

case class Prop(run: (TestCases,RNG) => Result) {
  

  def &&(p: Prop): Prop = Prop {
    (t,rng) => {
      val result1 = run(t,rng)
      val result2 = p.run(t,rng)
      (result1, result2) match {
        case (Passed,Passed) => Passed
        case (Passed, Falsified(_,_)) => result2
        case (Falsified(_,_), Passed) => result1
        case (Falsified(f1,s1), Falsified(f2,s2)) => Falsified(f1+ ";" + f2,s1+s2)
      }
    } 
  }   
  
  def ||(p: Prop): Prop = Prop {
    (t,rng) => {
      val result1 = run(t,rng)
      val result2 = p.run(t,rng)
      (result1, result2) match {
        case (Passed,Passed) => Passed
        case (Passed, Falsified(_,_)) => Passed
        case (Falsified(_,_), Passed) => Passed
        case (Falsified(f1,s1), Falsified(f2,s2)) => Falsified(f1+ ";" + f2,s1+s2)
      }
    } 
  }  
}

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  def isFalsified = false
}      
  
case class Falsified(failure: FailedCases, success: SuccessCount) extends Result {
  def isFalsified = true
}

object Prop {
  type MaxSize = Int
  type FailedCases = String
  type SuccessCount = Int
  type TestCases = Int

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a,i) => try {
          if(f(a))Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMessage(a,e), i) }
      
    }.find(_.isFalsified).getOrElse(Passed)
  }
  
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = 
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))
  
  def buildMessage[A](s: A, e: Exception): String = 
    s"test case: ${s}\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
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
    
  def double: Gen[Double] = Gen{State{RNG.double}}
  
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = 
    List.fill(n)(g).foldRight(unit(Nil: List[A]))((ga,gbs) => gbs.flatMap(bs => ga.map(a => a::bs)))
    
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = 
    weighted((g1,0.5), (g2,0.5))
    
  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A], Double)): Gen[A] = 
    for {
      a1 <- g1._1;
      a2 <- g2._1;
      d <- double
    } yield (if(g1._2 != 0 && d <= g1._2) a1 else a2)
    
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen {
    Gen.listOfN(_: Int,g)
  }  
    
    
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
    
  def unsized: SGen[A] = SGen { i => this }
    
  
  
}

case class SGen[A](forSize: Int => Gen[A]) {
  
}



