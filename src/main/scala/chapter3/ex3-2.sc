/**
 * Implement the function tail for removing the first element of a List. You can use sys.error("message")
 * to throw an execution if the List is Nil.
 * In the next chapter, we'll look at different ways of handling errors.
 * Be careful to use List enum and the Nil case defined here and not the built-in Scala List and Nil types
 */

enum List[+A]:
  case Nil
  case Cons(head: A, tail: List[A])

object List:
  def apply[A](as: A*): List[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))


def tail[A](as: List[A]): List[A] =
  as match
    case Cons(h1, t1) => t1
    case Nil => throw new RuntimeException("Error")

