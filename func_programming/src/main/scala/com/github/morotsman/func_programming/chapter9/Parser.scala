package com.github.morotsman.func_programming.chapter9

import com.github.morotsman.func_programming.chapter8.Gen
import com.github.morotsman.func_programming.chapter8.Prop

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait

  def char(c: Char): Parser[Char] = 
    string(c.toString).map(_.charAt(0))

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

  implicit def string(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = 
    if(n == 0){
      succeed(List())
    }else {
      map2(p,listOfN(n-1,p))((a,b) => a::b)
    }
  
  def many[A](p: Parser[A]): Parser[List[A]] = 
    map2(p,many(p))((a,b) => a::b).or(succeed(List()))

    

  def map[A, B](p: Parser[A])(f: A => B): Parser[B]
  
  def succeed[A](a: A): Parser[A] = //unit
    string("").map(_ => a)
    
  def slice[A](p: Parser[A]): Parser[String]
  
  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)]
  
  def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = 
    product(p1,p2).map(ab => f(ab._1, ab._2))
    
  def many1[A](p: Parser[A]): Parser[List[A]] = 
    map2(p,many(p))((a1,a2) => a1 :: a2)

    
  //number of zero or more a's
  val numA: Parser[Int] = char('a').many.map(_.size)
  
  //zero or more a's followed by one or more b's
  val aAndB = char('a').many.slice.map(_.size) ** char('b').many1.slice.map(_.size) 
    

  case class ParserOps[A](p: Parser[A]) {

    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def many = self.many(p)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    
    def slice[A]= self.slice(p)
    
     def **[B](p2: => Parser[B]): Parser[(A,B)] =
      self.product(p,p2)
      
    def product[B](p2: => Parser[B]): Parser[(A,B)] =
      self.product(p,p2)
    
    def many1 = self.many1(p)
    
  }

  object Laws {

    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    def charLaw(p: Parser[Char])(in: Gen[Char]): Prop =
      Prop.forAll(in)(c => run(p)(c.toString) == succeed(c)) 
      
    def stringLaw(p: Parser[String])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(p)(s) == succeed(s))
      
    def succeedLaw[A](p: Parser[A])(in: Gen[A]): Prop = 
      Prop.forAll(in)(s => run(succeed('a'))(s.toString) == Right('a'))
      

    //
    //laws
    //run(or(string(s1),string(s2)))(s1) == Right(s1)
    //run(or(string(s1),string(s2)))(s2) == Right(s2)
    //run(or(string(s1),string(s2)))(s3) == Left(error)
    //run(listOfN(3,"ab" | "cad"))("ababcad") = Right("ababcad")
    //run(listOfN(3,"ab" | "cad"))("cadabab") = Right("cadabab")
    //run(listOfN(3,"ab" | "cad"))("ababab") = Right("ababab")

  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col = input.slice(0, offset + 1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset + n)

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) input.lines.drop(line - 1).next
    else ""
}

case class ParseError(stack: List[(Location, String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}

