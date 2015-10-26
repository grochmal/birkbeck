#!/usr/bin/env scala
!#

// http://www.scala-lang.org/docu/files/api/scala/Array.html was used
// extensively to produce the code below, although:
// I did not find the need to use .map in the exercises;
// Also, the calls to .fold can be easily changed to .reduceLeft, but
// the advantage of using .fold lies in the possibility of processing
// arrays of length 1.

// returns a function that evaluates if an integer can be a factor of x,
// note that 1 is not a factor of any number by definition
def isFactorOf(x :Int): (Int => Boolean) = {
  def isFactorOfx(y: Int): Boolean = 0 == x.abs % y.abs && y.abs != 1
  isFactorOfx
}

// returns an array of all positive integers from 0 to x
// or an empty array if the argument is negative or zero
def arrayUpTo(x: Int, arr: Array[Int] = Array()): Array[Int] = x match {
  case 0 => arr
  case y if y < 0 => arr
  case _ => arrayUpTo(x - 1, x +: arr)
}

// Returns an array of all factors of a natural number
// or an empty array where factorisation is not defined (zero and
// negative numbers).  It is a generalisation of exercise 1
def allFactOf(x: Int): Array[Int] = arrayUpTo(x).filter(isFactorOf(x))

// evaluates if given integer is prime
def isPrime(x: Int): Boolean = 1 == allFactOf(x).length

// given an array of integers multiply all it's elements
def product(arr: Array[Int]): Int = arr.fold(1){ (x, y) => x * y }

// given an arry of integers returns the largest integer in the array,
// throws NoSuchElementException if called with an empty array
def largest(arr: Array[Int]): Int = arr match {
  case array if 1 >= array.length => arr.head
  case _ => arr.tail.fold(arr.head){ (x, y) => if (x > y) x else y }
}

// returns the largest prime number in arr, if there are no prime numbers
// in arr it throws NoSuchElementException instead
def largestPrime(arr: Array[Int]): Int = largest(arr.filter(isPrime))

// Checks whether array arr contains any factor of number x,
// we consider that the number x is not a factor of itself.
// reverse.tail on the array of all factors give us all the factors
// of a number but not the number itself.
def haveFactOf(x:Int, arr: Array[Int]): Boolean = x match {
  case x if 1 < x => allFactOf(x).reverse.tail.exists(arr.contains(_))
  case _ => false
}

// Keeps in the array only the numbers that have factors of themselves
// within the same array.  Note that calling retainMultiples repeatedly
// on the same array produces a shorter array each time.
def retainMultiples(arr: Array[Int]): Array[Int] =
  arr.filter({ (x) => haveFactOf(x, arr) })

// here be dragons (unit tests)
val factorOf2 = isFactorOf(2)
val factorOf3 = isFactorOf(3)
assert(true  == factorOf2(2), "E: factorOf2(2) is false")
assert(false == factorOf2(3), "E: factorOf2(3) is true" )
assert(true  == factorOf3(3), "E: factorOf3(3) is false")
assert(false == factorOf3(2), "E: factorOf3(2) is true" )

assert( Array(1,2,3).deep        == arrayUpTo( 3).deep, "E: arrayUpTo(3)" )
assert( Array(1,2).deep          == arrayUpTo( 2).deep, "E: arrayUpTo(2)" )
assert( Array(1).deep            == arrayUpTo( 1).deep, "E: arrayUpTo(1)" )
assert((Array():Array[Int]).deep == arrayUpTo( 0).deep, "E: arrayUpTo(0)" )
assert((Array():Array[Int]).deep == arrayUpTo(-1).deep, "E: arrayUpTo(-1)")

assert( Array(2).deep            == allFactOf( 2).deep, "E: allFactOf(2)" )
assert( Array(3).deep            == allFactOf( 3).deep, "E: allFactOf(3)" )
assert( Array(2,4).deep          == allFactOf( 4).deep, "E: allFactOf(4)" )
assert( Array(2,3,6).deep        == allFactOf( 6).deep, "E: allFactOf(6)" )
assert( Array(7).deep            == allFactOf( 7).deep, "E: allFactOf(7)" )
assert( Array(3,9).deep          == allFactOf( 9).deep, "E: allFactOf(9)" )
assert((Array():Array[Int]).deep == allFactOf( 1).deep, "E: allFactOf(1)" )
assert((Array():Array[Int]).deep == allFactOf( 0).deep, "E: allFactOf(0)" )
assert((Array():Array[Int]).deep == allFactOf(-1).deep, "E: allFactOf(-1)")

assert(! isPrime(-2), "E: isPrime(-2)")
assert(! isPrime(-1), "E: isPrime(-1)")
assert(! isPrime( 0), "E: isPrime(0)" )
assert(! isPrime( 1), "E: isPrime(1)" )
assert(  isPrime( 2), "E: isPrime(2)" )
assert(  isPrime( 3), "E: isPrime(3)" )
assert(! isPrime( 4), "E: isPrime(4)" )
assert(  isPrime( 5), "E: isPrime(5)" )
assert(! isPrime( 6), "E: isPrime(6)" )
assert(  isPrime( 7), "E: isPrime(7)" )
assert(! isPrime( 8), "E: isPrime(8)" )
assert(! isPrime( 9), "E: isPrime(9)" )
assert(! isPrime(10), "E: isPrime(10)")

assert( 1 == product(Array(1)),           "E: product 1"      )
assert( 2 == product(Array(2)),           "E: product 2"      )
assert( 2 == product(Array(1,2)),         "E: product 1, 2"   )
assert( 6 == product(Array(1,2,3)),       "E: product 1, 2, 3")
assert(18 == product(Array(3,2,3)),       "E: product 3, 2, 3")
assert(18 == product(Array(6,3)),         "E: product 6, 3"   )
assert(-6 == product(Array(2,-3)),        "E: product 2, -3"  )
assert( 6 == product(Array(-2,-3)),       "E: product -2, -3" )
assert( 1 == product(Array():Array[Int]), "E: product Nil"    )

assert( 1 == largest(Array(1)),           "E: largest 1"             )
assert( 2 == largest(Array(1,2)),         "E: largest 1, 2"          )
assert( 6 == largest(Array(3,2,6)),       "E: largest 3, 2, 6"       )
assert( 1 == largest(Array(1,-2)),        "E: largest 1, -2"         )
assert( 0 == largest(Array(-1,0,-2)),     "E: largest -1, 0, -2"     )
assert(-3 == largest(Array(-7,-3,-3,-4)), "E: largest -7, -3, -3, -4")
assert(12 == largest(Array(12,0,-3,-2)),  "E: largest 12, 0, -3, -2" )

assert(3 == largestPrime(Array(1,2,3)),   "E: largestPrime 1, 2, 3"   )
assert(3 == largestPrime(Array(8,3,6)),   "E: largestPrime 8, 3, 6"   )
assert(2 == largestPrime(Array(2,-3,6)),  "E: largestPrime 2, -3, 6"  )
assert(7 == largestPrime(Array(12,3,7)),  "E: largestPrime 12, 3, 7"  )
assert(2 == largestPrime(Array(1,2,2,1)), "E: largestPrime 1, 2, 2, 1")

assert(true  == haveFactOf( 4, Array(1,2)),   "E: haveFactOf 4 in 1, 2"    )
assert(false == haveFactOf( 4, Array(1,3)),   "E: haveFactOf 4 in 1, 3"    )
assert(true  == haveFactOf( 6, Array(1,3)),   "E: haveFactOf 6 in 1, 3"    )
assert(true  == haveFactOf( 6, Array(1,2,3)), "E: haveFactOf 6 in 1, 2, 3" )
assert(true  == haveFactOf(12, Array(1,2,3)), "E: haveFactOf 12 in 1, 2, 3")
assert(false == haveFactOf( 6, Array(12,18)), "E: haveFactOf 6 in 12, 18"  )
assert(false == haveFactOf( 2, Array(1,4)),   "E: haveFactOf 2 in 1, 4"    )
assert(false == haveFactOf( 1, Array(1,3)),   "E: haveFactOf 1 in 1, 3"    )
assert(false == haveFactOf( 0, Array(1,3)),   "E: haveFactOf 0 in 1, 3"    )
assert(false == haveFactOf(-1, Array(1,3)),   "E: haveFactOf -1 in 1, 3"   )

val rm = { (x) => retainMultiples(x) }  // to keep the tests short
assert( Array(6).deep            == rm(Array(1,3,6)).deep,    "E: rm 1,3,6"   )
assert( Array(6,12).deep         == rm(Array(3,6,8,12)).deep, "E: rm 3,6,8,12")
assert( Array(12).deep           == rm(Array(6,8,12)).deep,   "E: rm 3,6,8,12")
assert( Array(8,4).deep          == rm(Array(8,4,2)).deep,    "E: rm 8,4,2"   )
assert((Array():Array[Int]).deep == rm(Array(6,7,9)).deep,    "E: rm 6,7,9"   )
assert((Array():Array[Int]).deep == rm(Array(0,0)).deep,      "E: rm 0,0"     )
assert((Array():Array[Int]).deep == rm(Array(0)).deep,        "E: rm 0"       )
assert((Array():Array[Int]).deep == rm(Array()).deep,         "E: rm Array()" )
assert((Array():Array[Int]).deep == rm(Array(-2,-4)).deep,    "E: rm -2,-4"   )

// tests from the coursework sheet
val array = Array(3, 4, 2, 5, 8)
assert(960 == product(array))
assert(5 == largestPrime(array))
assert(Array(4,8).deep == retainMultiples(array).deep)

