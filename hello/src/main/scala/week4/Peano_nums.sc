abstract class Nat {
  def isZero: Boolean

  def predecessor: Nat

  def successor: Nat

  def +(that: Nat): Nat

  def -(that: Nat): Nat
}

object Zero extends Nat {
  override def isZero: Boolean = true

  override def successor: Nat = new Succ(Zero)

  override def +(that: Nat): Nat = that

  override def -(that: Nat): Nat =
    if (that.isZero)
      this
    else
      throw new Exception("Only positive numbers")

  override def predecessor: Nat = throw new Exception("Only positive numbers")
}

class Succ(n: Nat) extends Nat {
  override def isZero: Boolean = false

  override def successor: Nat = new Succ(this)

  override def +(that: Nat): Nat = new Succ(n + that)

  override def -(that: Nat): Nat = n - that.predecessor

  override def predecessor: Nat = n
}

val b : Nat = new Succ(Zero)
val c = b - new Succ(Zero)
assert(c.isZero)
