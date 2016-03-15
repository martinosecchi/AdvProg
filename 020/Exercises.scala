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

// An ADT of Lists

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {

  // override function application to provide a factory of lists (convenience)

  def apply[A](as: A*): List[A] = // Variadic function
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Exercise 2
	
  def tail[A] (as: List[A]) :List[A] = as match {
	case Nil => Nil
	case Cons(h, t) => t
  } 

  // Exercise 3

  def setHead[A] (as: List[A], newHead: A) : List[A] = as match {
	case Nil => Cons(newHead, Nil)
	case Cons(h, t) => Cons(newHead, t)
  }

  // Exercise 4

  def drop[A] (l: List[A], n: Int) : List[A] =
	  if (n == 0) l
	  else drop(tail(l), n-1)

  // Exercise 5

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
	  case Nil => Nil
	  case Cons(h, t) => if(f(h)) dropWhile(t, f) else Cons(h, dropWhile(t, f))
  }

  // Exercise 6
  // Linear time and stack space

  def init[A](l: List[A]): List[A] = l match {
	  case Nil => Nil
	  case Cons(h, Nil) => Nil 
	  case Cons(h, t) => Cons(h, init(t))
  }

  // Exercise 7 is in the bottom of the file

  // Exercise 8

  def foldRight[A,B] (as :List[A], z: B) (f : (A,B)=> B): B = as match {
    case Nil => z
    case Cons (x,xs) => f (x, foldRight (xs,z) (f))
  }

  def length[A] (as: List[A]): Int = foldRight(as, 0)((x: A, y: Int) => y + 1) 

  // Exercise 9

  @annotation.tailrec	
  def foldLeft[A,B] (as: List[A], z: B) (f: (B, A) => B): B = as match {
	  case Nil => z
	  case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  } 

  // Exercise 10

  def sum (as : List[Int]) : Int = foldLeft(as, 0)(_+_) 
  def product (as : List[Int]) : Int = foldLeft(as, 1)(_*_) 
  def length1 (as :List[Int]) : Int = foldLeft(as, 0)((x:Int, y:Int) => x + 1) 

  // Exercise 11

  // Is this the right way to do it?
  def reverse[A] (as :List[A]) :List[A] = foldLeft(as, Nil: List[A])((y: List[A], x: A) => Cons(x, y))

  // Exercise 12

  // def foldRight1[A,B] (as: List[A], z: B) (f: (A, B) => B) : B = ...

  // def foldLeft1[A,B] (as: List[A], z: B) (f: (B,A) => B) : B = ...

  // Exercise 13

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
  }

  def concat[A] (as: List[List[A]]) :List[A] = foldRight(as, Nil: List[A])((r: List[A], s: List[A]) => append(r, s))


  // Exercise 14
	// It is not in tail position because of the nesting of the calls to the list constructors

	def map[A,B] (a: List[A]) (f: A => B): List[B] = reverse(foldLeft(a, Nil: List[B])((xs: List[B], x:A) => Cons(f(x), xs)))

  // Exercise 15 (no coding)
  // Foldleft would reverse the elements. The list constructor works in the "direction" of foldRight, prepending new elements to the head of the list 

  // Exercise 16

  def filter[A] (as: List[A]) (f: A => Boolean) : List[A] = foldRight(as, Nil: List[A])((x: A, xs: List[A]) => if(f(x)) Cons(x, xs) else xs)	
  // Exercise 17

  def flatMap[A,B](as: List[A])(f: A => List[B]) : List[B] = concat(map(as)(f)) 

  // Exercise 18

  def filter1[A] (l: List[A]) (p: A => Boolean) :List[A] = flatMap[A, A](l)((a: A) => if(p(a)) Cons(a, Nil) else Nil) 

  // Exercise 19
	// Notice me senpai
  def add (l: List[Int]) (r: List[Int]): List[Int] = (l,r) match {
	case (Nil, _) => Nil
	case (_, Nil) => Nil
	case (Cons(x, xs), Cons(y, ys)) => Cons(x+y, add(xs)(ys)) 
  }

  // Exercise 20

  def zipWith[A,B,C] (f : (A,B)=>C) (l: List[A], r: List[B]) : List[C] = (l,r) match {
	case (Nil, _) => Nil
	case (_, Nil) => Nil
	case (Cons(x, xs), Cons(y, ys)) => Cons(f(x,y), zipWith(f)(xs,ys)) 
  }

  // Exercise 21

  // def hasSubsequence[A] (sup: List[A], sub: List[A]) :Boolean = ...

  // Exercise 22

  def pascal (n :Int) : List[Int] = 
	  if (n == 1) Cons(1, Nil) 
	  else add(Cons(0, pascal(n-1)))(append(pascal(n-1), Cons(0, Nil))) 

  // a test: pascal (4) = Cons(1,Cons(3,Cons(3,Cons(1,Nil))))
}



// Exercise 7

object Exercise7 {

  case class SalaryLine(name: String, amount: Integer)

  def maximumSalary (salaries: List[SalaryLine]) :Integer = salaries match {
	  case Nil => -1
	  case Cons(h, t) => Math.max(h.amount, maximumSalary(t))
  }

  val test_case = List( SalaryLine("John",41),
    SalaryLine("Alice", 42),
    SalaryLine("Bob",40))

}


