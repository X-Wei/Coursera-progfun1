trait List[T]{
  def isEmpty : Boolean
  def head: T
  def tail: List[T]
}

class Cons[T](val head:T, val tail: List[T]) extends List[T] {
  def isEmpty(): Boolean = false
  // head and tail are implemented in the parameters(fields)
}

class Nil[T] extends List[T]{
  def isEmpty = false
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

def nth[T](n: Int, list:List[T]): T =
  if(list.isEmpty) throw new IndexOutOfBoundsException()
  else if (n==0) list.head
  else nth(n-1, list.tail)


