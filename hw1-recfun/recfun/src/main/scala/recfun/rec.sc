def pascal(c: Int, r: Int): Int = {
  if(c==0 || c==r) 1
  else pascal(c, r-1) + pascal(c-1, r-1)
}

pascal(0,0)

pascal(0,1)

pascal(1,3)

pascal(1,2)

def balance(chars: List[Char]): Boolean = {
  def balance_rec(chars: List[Char], acc: Int): Boolean = {
    if (acc<0) false
    else if (chars.isEmpty) acc == 0
    else if (chars.head == '(') balance_rec(chars.tail, acc+1)
    else if (chars.head == ')') balance_rec(chars.tail, acc-1)
    else balance_rec(chars.tail, acc)
  }

  balance_rec(chars, 0)
}

balance("(if (zero? x) max (/ 1 x))".toList)

balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList)

balance("())(".toList)

0 to 15/5
for( i <- 0 to 3)
  println(i)

