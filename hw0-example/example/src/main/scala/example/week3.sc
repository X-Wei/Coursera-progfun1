abstract class IntSet {
  def incl(x: Int): IntSet

  def contains(x: Int): Boolean

  def union(other: IntSet): IntSet
}



object Empty extends IntSet {
  override def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)

  override def contains(x: Int): Boolean = false

  override def toString = "."

  override def union(other: IntSet): IntSet = other
}

class NonEmpty(val elem: Int, val left: IntSet, val right: IntSet) extends IntSet {
  override def incl(x: Int): IntSet =
    if (x == elem) this
    else if (elem > x) new NonEmpty(elem, left.incl(x), right) // immutable!
    else new NonEmpty(elem, left, right.incl(x))

  override def contains(x: Int): Boolean =
    if (elem == x) true
    else if (elem > x) left.contains(x)
    else right.contains(x)

  override def toString() = "{" + left + elem + right + "}"

  override def union(other: IntSet): IntSet =
//    left.union(right).union(other).incl(elem)
    ((left union right ) union other ) incl elem
//    (left union right ) union (other incl elem)
//    ((other incl(elem) ) union left) union right
//    other.incl(elem).union(left).union(right)

}

val t1 = new NonEmpty(3, Empty, Empty)
val t2 = t1 incl 4
val t3 = new NonEmpty(2, Empty, Empty)
t3 union t2


