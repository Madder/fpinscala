package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }



  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (a, nextRNG) = int(rng)
    val nonNegA = if (a<0) (-a-1) else a
    (nonNegA, nextRNG)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (a, nextRNG) = nonNegativeInt(rng)
    val d = (a.toDouble/(Int.MaxValue.toDouble+1))
    (d, nextRNG)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, nextRNG) = int(rng)
    val (d, nextRNG2) = double(nextRNG)
    ((i,d),nextRNG2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i,d), nextRNG) = intDouble(rng)
    ((d,i),nextRNG)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, nextRNG1) = double(rng)
    val (d2, nextRNG2) = double(nextRNG1)
    val (d3, nextRNG3) = double(nextRNG2)
    ((d1,d2,d3), nextRNG3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count <= 0) (Nil, rng) else {
      val (x, nextRNG) = int(rng)
      val (xs, nextRNG2) = ints(count-1)(nextRNG)
      ((x::xs),nextRNG2)
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i%2)
  def doubleViaMap: Rand[Double] = map(nonNegativeInt)(i => i/(Int.MaxValue.toDouble+1))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a,rng1) = ra(rng)
    val (b,rng2) = rb(rng1)
    (f(a,b),rng2)
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra,rb)((_,_))
  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)
  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs match {
    case Nil => (rng => (Nil, rng))
    case randA::t => map2(randA,sequence(t))(_::_)
  }

  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(Nil:List[A]))((randA,acc) => map2(randA,acc)(_::_))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    sys.error("todo")
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    sys.error("todo")
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}

object stateTest {
  def main(args: Array[String]) {
    println(RNG.Simple(27736098).nextInt)
    println(Int.MaxValue)
    println(Int.MinValue)
    println(RNG.ints(10)(RNG.Simple(154)))
  }
}

object ThreadsCreation extends App {
  class MyThread extends Thread {
    override def run(): Unit = {
      Thread.sleep(1000)
      println("New thread running.")
      Thread.sleep(1000)
      println("Still running.")
      Thread.sleep(1000)
      println("Completed.")
    }
  }
  val t = new MyThread
  t.start()
  t.join()
  println("New thread joined.")
}


