package fpinscala.laziness

import Stream._
trait Stream[+A] {


  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }



  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  //Mine
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(hd, tl) => hd()::(tl().toList)
  }
  //From answers
  def toList_2: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h,t) => go(t(), h() :: acc)
      case _ => acc
    }
    go(this, List()).reverse
  }

  /*
  In order to avoid the `reverse` at the end, we could write it using a
  mutable list buffer and an explicit loop instead. Note that the mutable
  list buffer never escapes our `toList` method, so this function is
  still _pure_.
  */
  def toListFast: List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    @annotation.tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h,t) =>
        buf += h()
        go(t())
      case _ => buf.toList
    }
    go(this)
  }

  // 1st impl (not stack safe)
  def take_mine(n: Int): Stream[A] = {
    def go(s: => Stream[A], m: Int) : Stream[A] = if (m<=0) s else s match {
      case Cons(h,t) => cons(h(), go(t(), m-1))
      case Empty => empty
    }
    go(this, n)
  }



  def drop(n: Int): Stream[A] = this match {
    case Cons(h,t) if n>0 => t().drop(n-1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty:Stream[A])((a,s) => if (!p(a)) s else cons(a,s))

  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h,t) =>  p(h()) && t().forAll(p)
    case Empty => true
  }

  def forAllViaFoldRight(f: A => Boolean): Boolean =
    foldRight(true)((a,b) => f(a) && b)

  def headOption: Option[A] = this match {
    case Cons(h,t) => Some(h())
    case Empty => None
  }

  def headOptionViaFoldRight: Option[A] =
    foldRight(None:Option[A])((a,_) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    foldRight(empty: Stream[B])((h,t) => cons(f(h),t))

  def filter(p: A => Boolean) : Stream[A] =
    foldRight(empty: Stream[A])((h,t) => if (p(h)) cons(h,t) else t)

  def append[B >: A](other: => Stream[B]): Stream[B] =
    foldRight(other:Stream[B])((h,acc)=> cons(h,acc))


  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty: Stream[B])((h,t) => f(h) append t)

  def findViaFilter(p: A => Boolean): Option[A] =
    filter(p).headOption

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")


  @annotation.tailrec
  final def foldLeft[B](z: => B)(f: ( => B, A) => B): B = {

    this match {
      case Cons(h, t) =>  t().foldLeft(f(z, h()))(f)
      case _ =>  z
    }
  }

  /*
Create a new Stream[A] from taking the n first elements from this. We can achieve that by recursively
calling take on the invoked tail of a cons cell. We make sure that the tail is not invoked unless
we need to, by handling the special case where n == 1 separately. If n == 0, we can avoid looking
at the stream at all.
*/
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }


}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))

  def fibs: Stream[Int] = {
    def go(cur: (Int, Int)): Stream[(Int, Int)] =
      Stream.cons(cur, go((cur._2, cur._1 + cur._2)))

    go(0,1).map(_._1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a,s)) => cons(a,unfold(s)(f))
    case None => empty
  }

  val onesViaUnfold =
    unfold(1)(_ => Some((1,1)))


}

object TestStream {
  import Stream._
  //var foldStep = 0
  //var takeStep = 0
  def main(args: Array[String]) {
    //var sumStep = 0
    //def sum(a: () =>Int, b: Int) = {println("sumStep Step "+ sumStep + " b= "+ b); sumStep = sumStep +1; a()+b}
    //val k :Int= ones.take(20).foldLeft(0)((a,b) => sum(() => a,b))
    //println(k)
    def f(x: => Int, y: Int) = {println("ha"); 2}
    println(fibs.take(20000).foldLeft(0)(f))
  }


  //def f : Int=>Int = i => i+1
  //def composepwr[A](f: A=> A)(n: Int): A=> A = if (n <= 0) f else (f compose composepwr(f)(n-1))
  //def g = composepwr(f)(30)
  //println("g defined:" + g)
  //println("g(0): " + g(0))
}