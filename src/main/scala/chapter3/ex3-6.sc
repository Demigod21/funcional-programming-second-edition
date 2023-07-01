/**
 * Not everything works out so nicely. Implement a function init that returns a List
 * consisting of all but the last element of a List, so given List(1,2,3,4), init will return
 * List(1,2,3). Why can't this function implemented in constant time like tail?
 *
 * def init[A](as: List[A]): List[A]
 *
 */

enum List[+A]:
  case Nil
  case Cons(head: A, tail: List[A])

object List:
  def apply[A](as: A*): List[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))


def init[A](as: List[A]): List[A] = as match{
  case Nil => sys.error("init of empty list")
  case Cons(hd, Nil) => Nil
  case Cons(hd, tl) => Cons(hd, init(tl))
}