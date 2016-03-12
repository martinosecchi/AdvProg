// Advanced Programming 2015
// Andrzej Wąsowski, IT University of Copenhagen
//
// A script meant to be loaded into REPL (scala -i Main.scala)

import fpinscala.laziness._
import fpinscala.laziness.Stream._

// this is how we do simple interactive testing

//val l1 :Stream[Int] = Empty
//val l2 :Stream[Int] = empty
//val l3 :Stream[Int]= cons(1, cons(2, cons (3, empty)))

val naturals = Stream.from(0)
val str = Stream( 1,2,3,4,5,6,7,8,9,10 )

//println (l1.headOption)
//println (l2.headOption)
//println (l3.headOption)

//println(naturals.headOption2())
//naturals.map (_*2).drop (30).take (50).toList
//naturals.drop(42).filter (_%2 ==0).take (30).toList
//naturals.append (naturals)
//naturals.take(10).append(naturals).take(20).toList
//naturals.flatMap (to _).take (100).toList
//naturals.flatMap (x =>from (x)).take (100).toList
//fibs.take(10).toList
//val naturals2 = unfold(0)( (x) => Some(x, x+1))
//Stream.from(1).take(1000000000).drop(41).take(10).toList == Stream.from2(1).take(1000000000).drop(41).take(10).toList
//Stream.fibs.take(100).toList == Stream.fibs2.take(100).toList
//str.takeWhile3(_<5).toList == str.takeWhile2(_<5).toList
//str.takeWhile3(_<5).toList
//str.take2(5).toList
//str.zipAll(str).toList
//naturals.zipWith[Int,Int] (_+_) (naturals).take(2000000000).take(20).map(_/2).toList
//str.startsWith( Stream(1,2,3,4,5))
//str.startsWith( Stream( 11, 24, 42, 3, 4, 5  ) )
//Stream(1,2,3).tails.map(_.toList).toList
