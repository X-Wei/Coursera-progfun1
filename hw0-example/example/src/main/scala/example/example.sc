object example {

  import scala.annotation.tailrec

  "Hello world"

  def sqrt(x: Double): Double = {
    def improve(guess: Double): Double =
      (x / guess + guess) / 2.0

    def isGoodGuess(guess: Double): Boolean =
      math.abs((guess * guess - x) / x) < 0.001

    def sqrtItr(guess: Double): Double = {
      if (isGoodGuess(guess)) guess
      else sqrtItr(improve(guess))
    }

    sqrtItr(1)
  }

  sqrt(2)

  sqrt(4)

  sqrt(1e-6)

  sqrt(1e60)

  3 * 8

  def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)

  gcd(14, 21)

  def factorial(n: Int): Int =
    if (n == 0) 1 else n * factorial(n - 1)

  factorial(10)


  def factorialTR(n: Int): Int = {
    @tailrec
    def fact(n: Int, acc: Int): Int = {
      if (n == 0) acc
      else fact(n - 1, n * acc)
    }
    fact(n, 1)
  }

  factorialTR(4)


  def sum(f: Int => Int, a: Int, b: Int): Int = {
    if (a > b) 0
    else f(a) + sum(f, a + 1, b)
  }

  sum(x => x, 1, 10)

  sum(x => x * x, 1, 10)

  def sum2(f: Int => Int, a: Int, b: Int): Int = {
    @tailrec
    def sumTR(a: Int, acc: Int): Int = {
      if (a > b) acc
      else sumTR(a + 1, acc + f(a))
    }
    sumTR(a, 0)
  }

  sum2(x => x, 1, 10)

  def sum3(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 0
    else f(a) + sum3(f)(a + 1, b)
  }

  def cube(x: Int) = x * x * x

  sum3(cube)(1, 10)

  def product(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 1
    else a * product(f)(a + 1, b)
  }

  def fact(n: Int) = product(x => x)(1, n)

  fact(10)

  def more_general(op: (Int, Int) => Int, default: Int)
                  (f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) default
    else op(f(a), more_general(op, default)(f)(a + 1, b))
  }

  more_general((x, y) => x + y, 0)(x => x * x)(1, 10)

  def map_reduce(f: Int => Int, op: (Int, Int) => Int, default: Int)
                (a: Int, b: Int): Int = {
    if (a > b) default
    else op(f(a), map_reduce(f, op, default)(a + 1, b))
  }

  def factorial2(n: Int): Int = map_reduce(x => x, (a, b) => a * b, 1)(1, n)

  factorial2(10)


  val tol = 0.001

  def isCloseEnough(x: Double, y: Double): Boolean =
    math.abs((x - y) / x) < tol

  def fixedPoint(f: Double => Double)(firstGuess: Double): Double = {
    def iterate(guess: Double): Double = {
      println(guess)
      if (isCloseEnough(guess, f(guess))) guess
      else iterate(f(guess))
    }
    iterate(firstGuess)
  }

  fixedPoint(x => 1 + x / 2)(1)

def avgDamping(f:Double=> Double)(x:Double):Double =
  (f(x)+x)/2
def sqrt2(x: Double): Double = fixedPoint(avgDamping(y=>x/y))(1)

  sqrt2(2)

}