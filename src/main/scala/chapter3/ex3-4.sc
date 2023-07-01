/**
 * Implement the function drop, which removes the first n elements from a list.
 * Dropping n elements from an empty list should return the empty list.
 * Note that this function takes time proportional only to the number of elements being dropped--we dont need to
 * make a copy of the entire list
 *
 * def drop[A](as: List[A], n: Int): List[A]
 *
 */
enum List[+A]:
  case Nil
  case Cons(head: A, tail: List[A])

object List:
  def apply[A](as: A*): List[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

def drop[A](as: List[A], n: Int): List[A] = {
  if n<= 0  then as
  else as match
    case Cons(hd, tl) => drop(tl, n - 1)
    case Nil => Nil
}