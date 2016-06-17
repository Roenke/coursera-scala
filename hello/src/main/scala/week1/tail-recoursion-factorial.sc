
def factorial(n: Integer): Integer = {
  def factorialIteration(acc: Integer, n: Integer): Integer = {
    if (n == 0) acc else factorialIteration(acc * n, n - 1)
  }

  factorialIteration(1, n)
}

factorial(5)
factorial(12)



