/**
 * Let's look at another example, currying, which converts a function f of two arguments
 * into a function of one argument that partially applies f. Here again there's only
 * one implementation that complies
 * Write this implementation
 *
 * def curry[A, B, C](f: (A, B) => C) : A => (B => C)
 *
 * Note that type A => (B => C) can be read as a function that takes an A and
 * return a new function from B to C
 *
 */

def curry[A, B, C](f: (A, B) => C) : A => (B => C) = {
  a => b => f(a, b)
}