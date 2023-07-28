/**
 * Write a fcuntio to convert a LazyList to a List, which will force its evaulation and let
 * you look at it in the REPL. You can convert to the regular List type in the standard
 * library, and you can place this and other functions that operate on a LazyList inside the LazyList enum
 *
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

def toList: List[A] = this match
  case Empty => Nil
  case Cons(h, t) => h :: t().toList