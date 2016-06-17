def product(f: Int => Int): (Int, Int) => Int = {
  def prod(a: Int, b: Int): Int = {
    if (a > b) 1
    else f(a) * prod(a + 1, b)
  }

  prod
}

def factorial(n: Int): Int = {
  product(x => x)(1, n)
}

product((x: Int) => x * x)(1, 3)
factorial(5)

def fold(f: (Int, Int) => Int, init: Int): (Int, Int) => Int = {
  def fun(a: Int, b: Int): Int = {
    if (a > b) init
    else f(a, fun(a + 1, b))
  }

  fun
}

fold((x, y) => x + y, 0)(1, 5)
fold((x, y) => x * y, 1)(1, 5)

def mapReduce(f: Int => Int, combine: (Int, Int) => Int, init: Int): (Int, Int) => Int = {
  def fun(a: Int, b: Int): Int = {
    if (a > b) init
    else combine(f(a), fun(a + 1, b))
  }

  fun
}

mapReduce(x => x * x, (x, y) => x + y, 0)(1, 2)

