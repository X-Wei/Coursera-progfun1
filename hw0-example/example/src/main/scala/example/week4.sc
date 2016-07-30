abstract class Nat {
  def isZero: Boolean

  def pred: Nat

  def succ: Nat = new Succ(this)

  def +(that: Nat): Nat

  def -(that: Nat): Nat
}

object Zero extends Nat {
  override def isZero = true

  override def pred = throw new Error("0.pred")

  override def +(that: Nat) = that

  override def -(that: Nat) = if (that.isZero) this else throw new Error("neg number")
}

class Succ(n: Nat) extends Nat {
  def isZero = false

  def pred = n

  override def +(that: Nat) = new Succ(n + that)

  override def +(that: Nat) = if(that.isZero) n else n - that.pred
}