/**
 * Using the same idea, impelement the function setHead for replacing the first element of a List with a different value
 */

enum List[+A]:
  case Nil
  case Cons(head: A, tail: List[A])

object List:
  def apply[A](as: A*): List[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

def setHead[A](as: List[A], newHead: A): List[A] =
  as match
    case Cons(h1, t1) => Cons(newHead, t1)
    case Nil => throw new RuntimeException("Error")