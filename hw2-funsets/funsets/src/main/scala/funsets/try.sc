class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be non zero")

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  def numer = x

  def denom = y

  def this(x: Int) = this(x, 1)

  def +(that: Rational) =
    new Rational(numer * that.denom + denom * that.numer, that.denom * denom)

  def unary_- = new Rational(-numer, denom)

  def -(that: Rational) =
    this + -that

  def < (that: Rational) =
    this.numer * that.denom < this.denom * that.numer

  def max(that: Rational) = if (this < that) that else this

  override def toString = {
    val g = gcd(numer, denom)
    numer/g + "/" + denom/g
  }
}

val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)

x + y
x < y
x max y
x - y - z


val strange = new Rational(1, 0)











