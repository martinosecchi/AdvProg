package advpro5

import scala.annotation.tailrec
import scala.collection.immutable.Stream.cons

trait RNG {
  def nextInt: (Int, RNG)
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

  // Exercise 1 (CB 6.1)

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val res = rng.nextInt
    if (res._1 < 0)
      ( - res._1 + 1, res._2 )
    else
      res
  }
  //for testing in REPL: RNG.nonNegativeInt(RNG.Simple(42))
  //test:
  val test1 = nonNegativeInt(Simple(42))

  // Exercise 2 (CB 6.2)

  def double(rng: RNG): (Double, RNG) = {
    val pos = nonNegativeInt(rng)
    (pos._1/(Int.MaxValue.toDouble + 1), pos._2)
  }

  val test2 = double(Simple(42))

  // Exercise 3 (CB 6.3)

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val int = rng.nextInt
    val dob = double(int._2)
    ((int._1, dob._1), dob._2)
  }
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val int = rng.nextInt
    val dob = double(int._2)
    ((dob._1, int._1), dob._2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val dob1 = double(rng)
    val dob2 = double(dob1._2)
    val dob3 = double(dob2._2)
    ((dob1._1,dob2._1,dob3._1), dob3._2)
  }

  // def boolean(rng: RNG): (Boolean, RNG) =
  //  rng.nextInt match { case (i,rng2) => (i%2==0,rng2) }

  // Exercise 4 (CB 6.4)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def helper(count: Int)(rng: RNG)(prog: List[Int]) : (List[Int], RNG) = {
      if (count > 0) {
        val r = rng.nextInt
        helper(count - 1)(r._2)(r._1::prog)
      } else {
        (prog, rng)
      }
    }
    helper(count)(rng)(List[Int]())
  }
  val test4 = ints(5)(Simple(42))

  def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count > 0){
      val r = rng.nextInt
      val list = ints2(count-1)(r._2)
      (r._1::list._1, list._2)
    } else {
      (List[Int](),rng)
    }
  }
  val testints2 = ints2(5)(Simple(42))

  // There is something terribly repetitive about passing the RNG along
  // every time. What could we do to eliminate some of this duplication
  // of effort?

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  // Exercise 5 (CB 6.5)

  val _double: Rand[Double] = map(nonNegativeInt)(i => i/(Int.MaxValue.toDouble +1))
  val test5 = _double(Simple(42))

  // Exercise 6 (CB 6.6)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val r1 = ra(rng)
      val r2 = rb(r1._2)
      ( f(r1._1, r2._1), r2._2)
    }
  }

  // this is given in the book

   def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

   val randIntDouble: Rand[(Int, Double)] = both(int, double)

   val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  val test6 = randIntDouble(Simple(42))

  // Exercise 7 (6.7)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs match {
      case h::t => rng => {
        val (hv, hs) = h(rng)
        val (tv, ts) = sequence(t)(hs) //state chaining
        ( hv :: tv, ts)
      }
      case Nil => rng => { (List[A](), rng)}
    }
  }

  def _ints(count: Int): Rand[List[Int]] = {
    sequence(List.fill( count )( _.nextInt ) )
  }
  val test7 = _ints(5)(Simple(42))

  // Exercise 8 (6.8)

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (fv, fs) = f(rng)
      g(fv)(fs)
    }
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt)( i => unit(i%n) )
  }

  val test8 = nonNegativeLessThan(10)(Simple(42))

  // Exercise 9 (6.9)

  def _map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)( i => unit(f(i)) )

  def _map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
  flatMap(ra)(a => flatMap(rb)( b => unit(f(a,b))))

  val test9 = _map2(unit(1), unit(2) )( (a,b) => a+b) (Simple(42))

}

case class State[S, +A](run: S => (A, S)) {

  // Exercise 10 (6.10)

  def map[B](f: A => B): State[S, B] =
    State( s => {
      val (a,s2) = run(s)
      (f(a),s2)
    })

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    State( s => {
      val (a,s2) = run(s)
      val (b, s3) = sb.run(s2)
      ( f(a,b), s3 )
    })

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a,s2) = run(s)
    f(a).run(s2)
  })

}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  // Exercise 10 (6.10) continued

  def sequence[S,A](sas: List[State[S, A]]): State[S, List[A]] = {
    sas match {
      case h::t => State( s => {
        val (hv,hs) = h.run(s)
        val (tv, ts) = sequence(t).run(hs)
        (hv::tv, ts)
      })
      case _ => State( s=> (List(),s))
    }
  }

  // This is given in the book:

  // def modify[S](f: S => S): State[S, Unit] = for {
  //   s <- get // Gets the current state and assigns it to `s`.
  //   _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  // } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))


  def random_int :Rand[Int] =  State (_.nextInt)

  // Exercise 11

  def state2stream[S,A] (s :State[S,A]) (seed :S) :Stream[A] = {
    val (aa,ss) = s.run(seed)
    cons(aa, state2stream( s )(ss))
    //using the same state but running it with different seeds produces different values
  }

  // Exercise 12
  // see the actual one in the Tests.scala file
  // val random_integers = State.state2stream( State.random_int )(RNG.Simple(42)).take(10).toList

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {

  // Exercise 13 (CB 6.11)

  def update(i : Input)(s: Machine) : Machine =
    (i,s) match {
      case (_, Machine(_, 0, _)) => s // out of candies -> ignore input
      case (Coin, Machine(false, _, _)) => s // ignore input -> coin in unlocked machine
      case (Coin, Machine(true, candy, coin)) => Machine(false, candy, coin + 1) //unlock machine, update coins
      case (Turn, Machine(true, _, _)) => s // ignore input -> trying to turn unlocked machine
      case (Turn, Machine(false, candy, coin)) => Machine(true, candy - 1, coin) // dispense a candy
    }

  // returns: n. of remaining candies, n.of coins + the current state of the machine
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    def helper( inputs: List[Input]) (s : Machine) : ((Int, Int), Machine) = {
      inputs match {
        case h::t => helper(t)( update(h)(s) )
        case Nil => ( (s.candies, s.coins), s )
      }
    }

    State[Machine, (Int,Int)]( s => {
      inputs match {
        case h::t => helper(inputs)(s)
        case Nil => ( (s.candies, s.coins) , s)
      }
    })
  }
}


// to test in REPL:

//import advpro5.RNG.Simple
//import advpro5._

//val test1 = RNG.nonNegativeInt(Simple(42))
//
//val test2 = RNG.double(Simple(42))
//
//val test3 = RNG.double3(Simple(42))
//
//val test4 = RNG.ints(5)(Simple(42))
//
//val testints2 = RNG.ints2(5)(Simple(42))
//
//val test5 = RNG._double(Simple(42))
//
//val test6 = RNG.randIntDouble(Simple(42))
//
//val test7 = RNG._ints(5)(Simple(42))
//
//val test8 = RNG.nonNegativeLessThan(10)(Simple(42))
//
//val test9 = RNG._map2(RNG.unit(1), RNG.unit(2) )( (a,b) => a+b) (Simple(42))
//
//val test10 = State.sequence( List(State.random_int, State.random_int, State.random_int) ).run(Simple(42))
//
//val test11 = State.state2stream( State.random_int )( RNG.Simple(42)).take(5).toList
//
//val random_integers = State.state2stream( State.random_int )(RNG.Simple(42)).take(10).toList
//
//val test13 = Candy.simulateMachine(List( Coin, Turn, Coin, Turn, Turn, Coin, Turn)).run(Machine(true, 10, 0))