// Advanced Programming
// Andrzej Wasowski, IT University of Copenhagen

// Martino Secchi

package fpinscala.laziness
import scala.language.higherKinds

import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers
import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary

// If you comment out all the import lines below, then you test the Scala
// Standard Library implementation of Streams. Interestingly, the standard
// library streams are stricter than those from the book, so some laziness tests
// fail on them :)

import stream00._    // uncomment to test the book solution
//import stream01._ // uncomment to test the broken headOption implementation
// import stream02._ // our version

class StreamSpecMsec extends FlatSpec with Checkers {

  import Stream._

  // An example generator of random finite non-empty streams
  def list2stream[A] (la :List[A]): Stream[A] = la.foldRight (empty[A]) (cons[A](_,_))

  // In ScalaTest we use the check method to switch to ScalaCheck's internal DSL
  def genNonEmptyStream[A] (implicit arbA :Arbitrary[A]) :Gen[Stream[A]] =
    for { la <- arbitrary[List[A]] suchThat (_.nonEmpty)}
      yield list2stream (la)

  def len[A](s: Stream[A]) :Int = s.foldRight(0)((_,s) => s+1)

  def forceCompareStreams[A](s1: Stream[A], s2: Stream[A]) : Boolean = {
    val a = s1.toList
    val b = s2.toList
    a.equals(b)
  }

  val ints = arbitrary[Int] suchThat (_ > 0)
  val streams = genNonEmptyStream[Int]
  val exceptionStream6 = cons(throw new scala.Exception(),
    cons(throw new scala.Exception(), cons(throw new scala.Exception(),
      cons(throw new scala.Exception(), cons(throw new scala.Exception(),
        cons(throw new scala.Exception(), empty))))))

  behavior of "headOption"

  // a scenario test:

  it should "return None on an empty Stream (01)" in {
    assert(empty.headOption == None)
  }

  // a property test:

  it should "return the head of the stream packaged in Some (02)" in check {
    // the implict makes the generator available in the context
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    ("singleton" |:
      Prop.forAll { (n :Int) => cons (n,empty).headOption == Some (n) } ) &&
    ("random" |:
      Prop.forAll { (s :Stream[Int]) => s.headOption != None } )

  }

  it should "not force the tail of the stream (03)" in {
    val str = cons(1, cons(throw new java.lang.Exception(), empty))
    str.headOption
  }

  behavior of "take"

//  - take should not force any heads nor any tails of the Stream it
//  manipulates

  it should "not force heads nor tails (11)" in {
    Stream(1,2,3).map(_/0).take(2)
  }

  //  - s.take(n).take(n) == s.take(n) for any Stream s and any n
  //    (idempotency)
  it should "respect idempotency (12)" in check {
    Prop.forAll (ints, genNonEmptyStream[Int]) { (n, s) =>
      forceCompareStreams(s.take(n).take(n), s.take(n)) }
  }

//  - take(n) does not force (n+1)st head ever (even if we force all
//  elements of take(n))
  it should "not force n+1 st head (13)" in {
    cons(1, cons(1, cons( throw new scala.Exception(), empty))).take(2)
  }

  behavior of "drop"

//  - s.drop(n).drop(m) == s.drop(n+m) for any n, m (additivity)
  it should "respect additivity (21)" in check {
    Prop.forAll(Gen.choose[Int](1, 10),
      Gen.choose[Int](1,10),
      genNonEmptyStream){(n,m,s) =>
      forceCompareStreams(s.drop(n).drop(m), s.drop(n+m))  //the tails are forced
    }
  }
//  - s.drop(n) does not force any of the dropped elements heads
  it should "not force dropped elements (22)" in {
    val test = cons(throw new scala.Exception(),
        cons( throw new scala.Exception(),
        cons(1, cons(2, cons(3, empty)))))
    assert(test.drop(2).toList.equals(List(1,2,3))) //toList forces the tail
  }
//    - the above should hold even if we force some stuff in the tail

  behavior of "map"

//  - x.map(id) == x (where id is the identity function)

//  - map terminates on infinite streams

  behavior of "append"

  //just to see how it works, we can remove that eventually
  it should "append" in {
    val s = cons(1,cons(2,empty))
    val ss = Stream(3,4)
    val ls = s.append(ss).toList
    //println(ls)
    assert(ls.equals(List(1,2,3,4)))
  }

  //some examples for append:

  // appending an empty stream should result in the starting stream s.append(Stream())==s

  // appending on a infinite stream should terminate ?
  // (e.g. Stream.constant(1) is an infinite stream ore Stream.from(0))

  // should not force the streams
  it should "not force the streams" in check {
    Prop.forAll(streams){ s => {
       cons(1,exceptionStream6).append(s)  // doesn't pass
      s.append(exceptionStream6)    // passes
      true
    } }
  }


}
