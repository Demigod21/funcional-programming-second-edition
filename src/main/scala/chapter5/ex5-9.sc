/**
 * Write a function that generates an infinite lazy list of integers starting from n, then n+1, n+2, and so on
 */

def fromn(n: Int): MyList[A] = cons(n, fromn(n + 1))