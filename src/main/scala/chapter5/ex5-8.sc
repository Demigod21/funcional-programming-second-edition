/**
 * Generalize ones sligthly to the function continaully, which returns an infinite
 * LazyList of a given value:
 *
 */

def continually[A](a: A): LazyList[A] =
  cons(a, continually(a))