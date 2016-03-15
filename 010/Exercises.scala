// Advanced Programming, Exercises by A. Wąsowski, IT University of Copenhagen
//
// AUTHOR1:
// AUTHOR2:
//
// Write names and ITU email addresses of both group members that contributed to
// the solution of the exercise (in alphabetical order by family name).
//
// You should work with the file by following the associated exercise sheet
// (available in PDF from the course website).
//
// The file is meant to be compiled as follows:
//
// scalac Exercises.scala
//
// or
//
// fsc Exercises.scala
//
// To run the compiled file do "scala Exercises"
//
// To load the file int the REPL start the 'scala' interpreter and issue the
// command ':load Exercises.scala'. Now you can interactively experiment with
// your code.
//
// Continue solving exercises in the order presented in the PDF file. Large
// parts of the file are commented in order to make sure that the exercise
// compiles.  Uncomment and complete fragments as you proceed.  The file shall
// always compile and run after you are done with each exercise (if you do them
// in order).  Please compile and test frequently.

// The extension of App allows writing statements at class top level (the so
// called default constructor). For App objects they will be executed as if they
// were placed in the main method in Java.

object Exercises extends App {

  // Exercise 3

	// Only the branch that returns 1 is in tail position. The other branches modify the return value and are therefore not in tail position.
	// Large values of n will result in roughly n stack frames. It is possible to imagine scenarios where this could be problematic.
	// For example if you are interested in computing a large power of a number very close to 1.
	
	def power (x: Double, n: Int) : Double = {
		if(n > 0) {
			if(n % 2 == 0) power(x, n/2)*power(x, n/2) else x*power(x,n-1)
		} else if(n == 0) {
			1
		} else {
			1/power(x, -n)
		}
	} 

  // A few tests, uncomment when your implementation is ready.

	assert (power (2.0, 2) == 4.0)
	assert (power (1.0, 42) == 1.0)
  //
  // The above assertions should pass when you call "scala Exercises".
  //
  // The following one should fail. Uncomment to check that assert works as
  // expected:
  //
  // assert (power (1.0, 42) == 2.0, "Failed assertion")

  // add 2-3 more tests:
	
	assert(power (2.0, -2) == 0.25)
	assert(power(50.0, 0) == 1.0)	


  // Exercise 4
	
	def fib(n: Int): Int = {
		@annotation.tailrec
		def go(n: Int, f0: Int, f1: Int): Int =	
			if(n <= 1) f0
			else go(n - 1, f1, f0 + f1)  
		go(n, 0, 1)
	}

  // some tests (uncomment, add more):

	assert (fib (1) == 0)
	assert (fib(22) == 10946)
	assert (fib(22) == fib(21) + fib(20))
  // Exercise 5

  // A simple object describing a cost line; implemented imperatively, Java
  // style (this way until we learn more Scala)
  class Expense {

    // A constructor definition
    def this (tag :String, price :Int) = {
      this()
      this.tag = tag
      this.price = price
    }

    var tag   :String = "" // a tag line in the accounting system
    var price :Int    = 0 // the price is in cents
  }

  // computes the total of expenses in cents

	def total(expenses: Array[Expense]) : Int = {
		@annotation.tailrec
		def cumulate(i: Int, sum: Int, a: Array[Expense]) : Int =
			if(i >= a.length) sum
			else cumulate(i + 1, sum + a(i).price, a)
		cumulate(0, 0, expenses)
	}	

  val testcase1 = Array[Expense](
    new Expense("Coffee", 450),
    new Expense("Cake", 350) )

   assert (total (testcase1) == 800) 

  val testcase2 = Array[Expense](
    new Expense("a", 1),
    new Expense("b", 2),
    new Expense("c", 3) )

   assert (total (testcase2) == 6) 

  // Exercise 6

	def isSorted[A](as: Array[A], ordered: (A,A) => Boolean) : Boolean = {
		@annotation.tailrec
		def testpair(i: Int) : Boolean =
			if(i >= as.length) true 
			else if(ordered(as(i - 1), as(i))) testpair(i + 1)
			else false
		testpair(1)
	}

  // some tests (uncomment)

	assert ( isSorted (Array(1,2,3,4,5,6), (a: Int, b: Int)=> a <= b))
	assert (!isSorted (Array(6,2,3,4,5,6), (a: Int, b: Int)=> a <= b))
	assert (!isSorted (Array(1,2,3,4,5,1), (a: Int, b: Int)=> a <= b))

  // add two tests with another type, for example an Array[String]

	assert (isSorted (Array("a", "b", "c"), (a: String, b: String) => a <= b))
	assert (isSorted (Array("a"), (a: String, b: String) => a <= b))


  // Exercise 7: a curried version of solution to exercise 3

	// def power1(x: Double) (n: Int) :Double = ...
	def power1(x: Double)(n: Int) : Double = {
		if(n > 0) {
			if(n % 2 == 0) power1(x)(n/2)*power1(x)(n/2) else x*power1(x)(n-1)
		} else if(n == 0) {
			1
		} else {
			1/power1(x)(-n)
		}
	}

  // Exercise 8

	def curry[A,B,C](f: (A,B) => C) : A => (B => C) = {
		(a: A) => ((b: B) => f(a, b)) 	
	}

  // test if it type checks by currying power automatically:

	val power_curried: Double => Int => Double = curry(power)

  // Exercise 9

	def uncurry[A,B,C](f: A => B => C) : (A,B) => C = {
		(a: A, b: B) => f(a)(b)
	}

	val power_uncurried: (Double,Int) => Double = uncurry(power_curried)

  // Exercise 10

	def compose[A,B,C] (f: B => C, g: A => B) : A => C = {
		(a: A) => f(g(a))
	}

}
