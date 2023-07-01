/**
 * Implement dropWhile which removes element from the List prefix as long
 * as they match a predicate
 *
 * def dropWhile[A](as: List[A], f: A => Boolean): List[A]
 *
 */

enum List[+A]:
  case Nil
  case Cons(head: A, tail: List[A])

object List:
  def apply[A](as: A*): List[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))


def dropWhile[A](as: List[A], f: A => Boolean): List[A] = {
       as match
        case Cons(hd, tl) if f(hd) => dropWhile(tl, f)
        case _ => as
    }