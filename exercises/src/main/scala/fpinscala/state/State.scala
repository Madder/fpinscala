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

  def sequenceViaFold[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(Nil:List[A]))((randA,acc) => map2(randA,acc)(_::_))

  def intsViaSequence(count: Int) : Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def nonNegativeLessThanBiased(n: Int): Rand[Int] =
    map(nonNegativeInt) { _ % n }


  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n-1) - mod >= 0)
      (mod, rng2)
    else nonNegativeLessThan(n)(rng2)
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng1) = f(rng)
    val (b, rng2) = g(a)(rng1)
    (b,rng2)
  }

  def nonNegativeLessThanViaFlatmap(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { a =>
      val mod = a % n
      if (a + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThanViaFlatmap(n)
    }

  def mapViaFlatmap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s){a=> unit(f(a))}


  def map2ViaFlatmap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => (map(rb)( b => f(a,b))))

}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    State { s=>
      val (a,s2) = run(s)
      (f(a),s2)
    }

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    State { s=>
      val (a,s2) = run(s)
      val (b,s3) = sb.run(s2)
      (f(a,b),s3)
    }
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State { s =>
      val (a,s2) = run(s)
      f(a).run(s2)
    }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {

  def unit[S,A](a: A): State[S,A] = State(s => (a, s))
  def sequence[S, A](fs: List[State[S,A]]): State[S,List[A]] =
    fs.foldRight(unit(Nil: List[A]): State[S,List[A]]) { (sa, acc) =>
      sa.map2(acc)(_::_)
    }

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

}

object MyCandy {
  def coin: State[Machine, (Int, Int)] = State { machine =>
    if (machine.locked && machine.candies>0)  ((machine.candies,machine.coins+1), Machine(false,machine.candies,machine.coins+1))
    else ((machine.candies,machine.coins),machine)
  }

  def turn: State[Machine, (Int, Int)] = State { machine =>
    if (machine.locked == false && machine.candies>0) ((machine.candies-1,machine.coins), Machine(false,machine.candies-1,machine.coins))
    else ((machine.candies,machine.coins),machine)
  }
  def state_transition(input: Input): State[Machine, (Int, Int)] = input match {
    case Coin => coin
    case Turn => turn
  }

  def id: State[Machine, (Int, Int)] = State { machine => ((machine.candies,machine.coins),machine)
  }
  type Rand[A] = State[RNG, A]

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = inputs match {
    case Nil => id
    case input::tail =>  state_transition(input) flatMap(_ => simulateMachine(tail))
  }
}
object Candy {
  import State._
  def update = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) =>
        Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(true, candy - 1, coin)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs.map(input => modify[Machine](update(input))) )
    s <- get
  } yield (s.coins, s.candies)
}

object FancyCandy {
  import State._
  def update = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) =>
        Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(true, candy - 1, coin)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    // modify[Machine] _ is the Eta expansion of modify[Machine] method
    _ <- sequence(inputs map (modify[Machine] _ compose update))
    s <- get
  } yield (s.coins, s.candies)
}




object stateTest {
  def main(args: Array[String]) {
    println(RNG.Simple(27736098).nextInt)
    println(Int.MaxValue)
    println(Int.MinValue)
    println(RNG.ints(10)(RNG.Simple(154)))
    println(RNG.intsViaSequence(10)(RNG.Simple(154)))
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


