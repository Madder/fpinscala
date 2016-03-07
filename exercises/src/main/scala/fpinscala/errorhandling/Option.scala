package fpinscala.errorhandling

import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case None => None
  }

  def flatMap_2[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def orElse[B>:A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  // !!!!!!!
  def orElse_2[B>:A](ob: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse ob


  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }

  def filter_2(f: A => Boolean): Option[A] =
    this flatMap(a => if (f(a)) Some(a) else None)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap { m => mean( xs.map(x => math.pow(x-m,2)) ) }

  def map2[A,B,C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] =
    fa flatMap { a => fb map { b => f(a,b)}}

  // my 1st attempt
  def map3_1[A,B,C,D](fa: Option[A], fb: Option[B], fc: Option[C])(f: (A, B, C) => D): Option[D] =
    fa flatMap { a => fb flatMap { b => fc map { c => f(a,b,c)}} }

  def map3_comprehention[A, B, C, D](fa: Option[A], fb: Option[B], fc: Option[C])(f: (A, B, C) => D): Option[D] = for {
    a <- fa
    b <- fb
    c <- fc
  } yield f(a, b, c)
  // my 2nd attempt, reusing map2, but still relying on flatMap :/
  def map3_2[A,B,C,D](fa: Option[A], fb: Option[B], fc: Option[C])(f: (A, B, C) => D): Option[D] = {
    def fcurried: (A,B)=>(C => D) = (a:A,b:B) => ((c:C) => f(a, b, c))
    map2(fa,fb)(fcurried) flatMap {fcur => fc map {c => fcur(c)}}
  }


  // From ch. 8!! Mm.. :)
  /** Implement map3, map4, and map5 in terms of map2 */
  def map3[A, B, C, D](fa: Option[A], fb: Option[B], fc: Option[C])(f: (A, B, C) => D): Option[D] = {
    map2(map2(fa, fb)((a, b) => (c: C) => f(a, b, c)), fc)(_(_))
  }

  def map4[A, B, C, D, E](fa: Option[A], fb: Option[B], fc: Option[C], fd: Option[D])(f: (A, B, C, D) => E): Option[E] = {
    map2(map2(map2(fa, fb)((a, b) => (c: C) => (d: D) => f(a, b, c, d)), fc)(_(_)), fd)(_(_))
  }

  def map5[A, B, C, D, E, F](fa: Option[A], fb: Option[B], fc: Option[C], fd: Option[D], fe: Option[E])(f: (A, B, C, D, E) => F): Option[F] = {
    map2(map2(map2(map2(fa, fb)((a, b) => (c: C) => (d: D) => (e: E) => f(a, b, c, d, e)), fc)(_(_)), fd)(_(_)), fe)(_(_))
  }


  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight(Some(List[A]()) : Option[List[A]])(map2(_,_)((a, la) => a::la))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight(Some(List[B]()) : Option[List[B]])((a, olb) => map2 (f(a),olb) ((b, lb) => b::lb))

  def traverse_slow[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    sequence (a map f)

}

object Test {
  def main(args: Array[String]) {
    println(Option.failingFn2(5))
    println("Omar")
    println(List(1,4,2,3).map(i => Option.Try(i / (i-1))))
    println(Option.traverse(List(1,4,2,3))(i => Option.Try(i / (i-1))))
  }
}