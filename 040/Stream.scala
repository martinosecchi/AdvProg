// Advanced Programming 2015
// Andrzej Wąsowski, IT University of Copenhagen
//
// meant to be compiled, for example: fsc Stream.scala

package fpinscala.laziness

import Stream._

sealed trait Stream[+A] {

  def headOption () :Option[A] =
    this match {
      case Empty => None
      case Cons(h,t) => Some(h())
    }

  def headOption2 () :Option[A] = {
    foldRight(None :Option[A])( (a,b) => Some(a))
  }

  def tail :Stream[A] = this match {
      case Empty => Empty
      case Cons(h,t) => t()
  }

  def foldRight[B] (z : =>B) (f :(A, =>B) => B) :B = this match {
      case Empty => z
      case Cons (h,t) => f (h(), t().foldRight (z) (f))
      // Note 1. f can return without forcing the tail
      // Note 2. this is not tail recursive (stack-safe) It uses a lot of stack
      // if f requires to go deeply into the stream. So folds sometimes may be
      // less useful than in the strict case
    }

  // Note 1. eager; cannot be used to work with infinite streams. So foldRight
  // is more useful with streams (somewhat opposite to strict lists)
  def foldLeft[B] (z : =>B) (f :(A, =>B) =>B) :B = this match {
      case Empty => z
      case Cons (h,t) => t().foldLeft (f (h(),z)) (f)
      // Note 2. even if f does not force z, foldLeft will continue to recurse
    }

  def exists (p : A => Boolean) :Boolean = this match {
      case Empty => false
      case Cons (h,t) => p(h()) || t().exists (p)
      // Note 1. lazy; tail is never forced if satisfying element found this is
      // because || is non-strict
      // Note 2. this is also tail recursive (because of the special semantics
      // of ||)
    }
  def toList : List[A] = {
    this match {
      case Empty => List()
      case Cons(h,t) => h() :: t().toList
    }
  }


  def take (n: Int) : Stream[A] = {
    this match {
      case Cons(h, t) => if (n > 1) Cons(h, () => t().take(n-1)) else Cons(h, () => Empty)
      case Empty => Empty
    }
  }

  def take2 (n: Int) : Stream[A] = {
    unfold( (this, n) ) ( (s) => {
      val ss = s._1
      val nn = s._2
       ss match {
        case Cons(h,t) => if ( nn > 0) Some(h(), (t(), nn -1)) else None
        case _ => None
      }
    })
  }

  def drop (n: Int) :Stream[A] = {
    this match {
      case Empty => Empty
      case Cons(h,t) => if (n > 1) t().drop( n-1 ) else t()
    }
  }

  def takeWhile (p: A => Boolean) : Stream[A] ={
    this match {
      case Cons(h,t) => if(p(h())) cons[A] (h(), t().takeWhile(p)) else empty
      case _ => empty
    }
  }
  //using foldRight
  def takeWhile2 (p: A => Boolean) : Stream[A] = {
    foldRight(empty[A])( (a,b) => if(p(a)) cons(a, b) else empty )
  }
  //using unfold
  def takeWhile3 (p: A => Boolean) : Stream[A] = {
    unfold(this){
      case Cons(h,t) => if(p(h())) Some(h(), t()) else None
      case _ => None
    }
  }

  def forAll(p: A => Boolean): Boolean = {
    this match {
      case Cons(h,t) => if(p(h())) t().forAll(p) else false
      case _ => true
    }
  }

  def map[B] (f : A => B) : Stream[B] = foldRight (empty[B])( (a,b) => cons(f(a), b))
  //using unfold
  def map2[B] (f : A => B) : Stream[B] = {
    //    this match {
    //      case Cons(h,t) => unfold(this)( (s) => Some( f(h()), t() ) ) //nope
    //      case Empty => empty
    //    }
    unfold(this) ({
      case Cons(h, t) => Some(f(h()), t())
      case Empty => None
    })
  }

  def filter (p: A => Boolean) :Stream[A] = foldRight(empty[A])( (a,b) => if (p(a)) cons(a,b) else b)

  def append[B>:A] (that : Stream[B]) : Stream[B] = { //using type Stream[A] gives contravariant position error
    foldRight(that)( (a,b) => cons(a,b))
  }

  def flatMap[B](f: A => Stream[B]) : Stream[B] = {
    foldRight(empty[B])( (a,b) => f(a).append(b))
  }

  def find (p :A => Boolean) :Option[A] = this.filter (p).headOption()
  //TODO efficient for streams but not for lists:
  // I don't really know, it seems to me that the values in the stream are evaluated during the filter, so it would be the same

  def zipWith[B, C] (f: (A,B) => C ) (that: Stream[B]) : Stream[C] = {
    unfold(this,that) {
      case ( Cons(h1,t1), Cons(h2,t2) ) => Some( f(h1(),h2()), (t1(),t2()) )
      case _ => None
    }
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = {
    unfold(this, s2) {
      case (Cons(h1,t1), Cons(h2,t2)) => Some( (Some(h1()), Some(h2())) , (t1(), t2() ) )
      case _ => None
    }
  }
  // result of naturals.map (_%2==0).zipWith[Boolean,Boolean] (_||_) (naturals.map (_%2==1)).take(10).t
  // should be: (true,false,true,false,true,false...) || (false, true,false,true ...) =>
  // a list of ten true Booleans
  //result of naturals.map (_%2==0).zipWith[Boolean,Boolean] (_&&_) (naturals.map (_%2==1)).take(10).t
  // should be : a list of 10 false Booleans

//easy pattern matching solution
  def startsWith[A] (that: Stream[A]) : Boolean ={
    (this, that) match {
      case ( Cons(h1,t1), Cons(h2,t2)) => if( h1() == h2() ) t1().startsWith(t2()) else false
      case ( Cons(h1,t1), Empty) => true
      case ( Empty, _) => false
    }
  }
//using unfold
  def tails : Stream[Stream[A]] = {
    unfold(this) ((s) => s match {
      case Cons(_, t) => Some( s, t())
      case Empty => None
    })
  }

}




case object Empty extends Stream[Nothing]
case class Cons[+A](h: ()=>A, t: ()=>Stream[A]) extends Stream[A]


object Stream {

  def empty[A]: Stream[A] = Empty

  def cons[A] (hd: => A, tl: => Stream[A]) :Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def apply[A] (as: A*) :Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
    // Note 1: ":_*" tells Scala to treat a list as multiple params
    // Note 2: pattern matching with :: does not seem to work with Seq, so we
    //         use a generic function API of Seq

  def from (n : Int) : Stream[Int] = {
    cons[Int] (n, from(n+1))
  }

  def to_reverse (n : Int) : Stream[Int] = {
    if (n >= 0){
      cons[Int] (n, to_reverse(n-1))
    } else {
      empty
    }
  }
  //in increasing order starting from 0
  def to (n: Int) :Stream[Int] = {
    def helper(n: Int, s: Int) :Stream[Int] = {
      if (s<=n) {
        cons[Int] (s, helper(n, s+1))
      } else {
        empty
      }
    }
    helper(n, 0)
  }

  def fib (a: Int, b: Int): Stream[Int] = cons(a, fib(b, a+b))
  val fibs = fib(0,1)

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    //f(z) -> Option[(A,S)] : produces next value of type A and next state of type S in an Option
    f(z) match {
      case Some((a,s)) => cons( a, unfold(s)(f))
      case None => empty
    }
  }

  def fib2 () : Stream[Int] = {
    // I manually add the first two elements 0 and 1
    cons( 0, cons( 1, unfold( (0,1) ) ( (a) => {
      val n = a._1 + a._2;
      Some( n, (a._2, n) )
    } )))
  }
  val fibs2 = fib2()

  def from2 (n : Int) : Stream[Int] =
    unfold(n)( (x) => Some(x, x+1) )

}

// vim:tw=0:cc=80:nowrap
