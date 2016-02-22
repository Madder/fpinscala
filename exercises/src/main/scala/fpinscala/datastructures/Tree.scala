package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {


  def size[A](t: Tree[A]) : Int = t match {
    case Leaf(a) => 1
    case Branch(t1, t2) => 1 + size(t1) + size(t2)
  }

  def maximum(t: Tree[Int]) : Int = t match {
    case Leaf(a) => a
    case Branch(t1, t2) => math.max(maximum(t1), maximum(t2))
  }

  def depth[A](t: Tree[A]) : Int = t match {
    case Leaf(a) => 0
    case Branch(t1, t2) => 1 + math.max(depth(t1), depth(t2))
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l,r) => g(fold(l)(f)(g),fold(r)(f)(g))
  }

}