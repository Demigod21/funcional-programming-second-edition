/**
 * Implement forAll which checks that all elements in the LazyList match a given predicate
 * Your implementation should terminate the traversal as soon as it encouters a non matching value
 */

def forAll(p: A => Boolean): Boolean =
  foldright(true)((a, b) => p(a) && b)