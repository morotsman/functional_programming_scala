package com.github.morotsman.func_programming.chapter3

sealed trait Tree[+A] {
  
  
  
  def size(): Int = this match {
    case Leaf(_) => 1
    case Branch(left,right) => 1 + left.size + right.size
  }
  
  def depth(): Int = this match {
    case Leaf(_) => 1
    case Branch(left,right) => 1 + left.depth.max(right.depth)
     
  }
  
  def map[B](f: A=>B): Tree[B] = this match {
    case Leaf(v) => Leaf(f(v))  
    case Branch(left,right) => Branch(left.map(f),right.map(f))
  }
  
  def fold[B](f: A => B)(g: (B,B) => B): B = this match {
    case Leaf(v) => f(v)
    case Branch(left,right) => g(left.fold(f)(g),right.fold(f)(g))  
  }
  
  def sizeInTermsOfFold(): Int =
    this.fold(v => 1)(1 + _ + _)
    
  def depthInTermsOfFold(): Int = 
    this.fold(v => 1)(1 + _.max(_))
    
    
  def mapInTermsOfFold[B](f: A=>B): Tree[B] = 
    this.fold(v => Leaf(f(v)) :Tree[B])((l,r) => Branch(l,r))
    
  
  
}


case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  
  def maximum(ls: Tree[Int]): Int = ls match {
    case Leaf(v) => v  
    case Branch(left,right) => {
      val maxLeft = maximum(left)
      val maxRight = maximum(right)
      maxLeft.max(maxRight)
    }
  }
  
  def maximumInTermsOfFold(ls: Tree[Int]): Int = 
    ls.fold(v => v)(_.max(_))

}