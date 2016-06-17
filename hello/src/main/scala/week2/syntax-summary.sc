object rationals {
  new Rational(1, 2)
}

val r = new Rational(1, 4)
r.denom

val r1 = new Rational(1, 2)
val r2 = new Rational(1, 4)
r1.sub(r2)

val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)
x.sub(y).sub(z)
x.add(x)

x add y

var two = new Rational(2)

class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be nonzero")

  def this(x: Int) = this(x, 1)

  private def gcd(x: Int, y: Int): Int = if (y == 0) x else gcd(y, x % y)

  private val g = gcd(x, y)

  def numer = x / g

  def denom = y / g

  def neg(): Rational = {
    new Rational(-x, y)
  }

  def add(other: Rational): Rational = {
    new Rational(numer * other.denom + denom * other.numer, denom * other.denom)
  }

  def sub(other: Rational): Rational = {
    add(other.neg())
  }

  override def toString: String = numer + " / " + denom
}
