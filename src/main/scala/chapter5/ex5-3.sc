/**
 * Write the function takeWhile for returning all starti elements of a LazyList that
 * match match the given predicate:
 *
 * def takeWhile(p : A => Boolean): LazyList[A]
 *
 */

def takeWhile(p : A => Boolean): LazyList[A] = this match
  case Cons(h, t) if (p(h) == true) => cons(h(), t().takeWhile(p))
  case _ => Empty