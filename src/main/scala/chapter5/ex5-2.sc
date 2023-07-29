/**
 * Write the function take(n) for returning the first n elements of a LazyList and
 * drop(n) for skipping the first n elements of a Lazy List
 */

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

object LazyList:
  def cons[A](hd: => A, tl : => LazyList[A]): LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => hd, () => tail)

////

def take(n: Int) : LazyList[A] = this match
  case Cons(h, t) if n > 1 => cons(h(), t().take(n-1))
  case Cons(h, t) if n == 1 => cons(h(), Empty)
  case _ => Empty

def drop(n: Int): LazyList[A] = this match
  case Cons(h, t) if n >= 1 => t().drop(n-1)
  case _ => this