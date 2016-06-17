def sum(f: Int => Int, a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a + 1, acc + f(a))
  }

  loop(a, 0)
}

sum((x: Int) => x, 1, 2)
sum((x: Int) => x * x, 2, 3)

def sum1(f: Int => Int): (Int, Int) => Int = {
  def sumF(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, acc + f(a))
    }

    loop(a, 0)
  }

  sumF
}

def id = (x:Int) => x
def sqr = (x:Int) => x * x

sum(id, 1, 100) == sum1(id)(1, 100)
sum(sqr, 1, 100) == sum1(sqr)(1, 100)