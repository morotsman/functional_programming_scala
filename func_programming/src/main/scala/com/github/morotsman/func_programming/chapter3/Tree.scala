package com.github.morotsman.func_programming.chapter3

sealed trait Tree[+A] {
  
  
  def size: Int = this match {
    case Leaf(_) => 1
    case Branch(l,r) => 1 + l.size + r .size
  }
  
  
  def sizeInTermsOfFold: Int = 
    this.fold(a => 1)(1 + _ + _)
    
  
  def depth: Int = this match {
    case Leaf(_) => 1
    case Branch(l,r) => 1 + (l.depth max r.depth)
  }
  
  def map[B](f : A => B): Tree[B] = this match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l,r) => Branch(l.map(f), r.map(f))
  }
  
  def fold[B](fl: A => B)(fb: (B,B) => B): B = this match {
    case Leaf(a) => fl(a)
    case Branch(l,r) => fb(l.fold(fl)(fb),r.fold(fl)(fb))
  }

  
  
}


case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  
  def maximum(ts: Tree[Int]) : Int = ts match {
    case Leaf(v) => v
    case Branch(l,r) => maximum(l) max maximum(r)
  }

}