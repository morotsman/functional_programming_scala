package com.github.morotsman.func_programming.chapter3

sealed trait Tree[+A] {
  
  
  
  def size(): Int = this match {
    case Leaf(_) => 1
    case Branch(l,r) => 1 + r.size() + l.size()
  } 
  
  def depth(): Int = this match {
    case Leaf(_) => 1
    case Branch(l,r) => 1 + (l.depth() max r.depth)
  } 
  
  def map[B](f: A=>B): Tree[B] = this match{
    case Leaf(v) => Leaf(f(v))
    case Branch(l,r) => Branch(l.map(f),r.map(f)) 
  }
  
  def fold[B](f: A => B)(g: (B,B) => B): B = this match {
    case Leaf(v) => f(v)
    case Branch(l,r) => g(l.fold(f)(g), r.fold(f)(g))
  }
  
  def sizeInTermsOfFold(): Int = 
    this.fold(_ => 1)(1 + _ + _)
    
  def depthInTermsOfFold(): Int = 
    this.fold(_ => 1)((l,r) => 1 + (l max r))
    
    
  def mapInTermsOfFold[B](f: A=>B): Tree[B] = 
    this.fold(v => Leaf(f(v)): Tree[B])((l,r) => Branch(l,r))
    
  
  
}


case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  
  def maximum(ls: Tree[Int]): Int = ls match{
    case Leaf(v) => v
    case Branch(l,r) => maximum(l) max maximum(r)
  }
  
  def maximumInTermsOfFold(ls: Tree[Int]): Int =
    ls.fold(v => v)((l,r) => l max r)

}