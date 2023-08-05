/**
 * Use foldRight to implement takeWhile
 */

def takeWhile(p : A => Boolean): LazyList[A] =
  foldRight(empty)((a, b) => if p(a) == true then cons(a, b) else empty)