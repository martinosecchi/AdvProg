
import advpro5.RNG.Simple
import advpro5._

//load file into the REPL

val test1 = RNG.nonNegativeInt(Simple(42))

val test2 = RNG.double(Simple(42))

val test3 = RNG.double3(Simple(42))

val test4 = RNG.ints(5)(Simple(42))

val testints2 = RNG.ints2(5)(Simple(42))

val test5 = RNG._double(Simple(42))

val test6 = RNG.randIntDouble(Simple(42))

val test7 = RNG._ints(5)(Simple(42))

val test8 = RNG.nonNegativeLessThan(10)(Simple(42))

val test9 = RNG._map2(RNG.unit(1), RNG.unit(2) )( (a,b) => a+b) (Simple(42))

val test10 = State.sequence( List(State.random_int, State.random_int, State.random_int) ).run(Simple(42))

val test11 = State.state2stream( State.random_int )( RNG.Simple(42)).take(5).toList

val random_integers = State.state2stream( State.random_int )(RNG.Simple(42)).take(10).toList

val test13 = Candy.simulateMachine(List( Coin, Turn, Coin, Turn, Turn, Coin, Turn)).run(Machine(true, 10, 0))


