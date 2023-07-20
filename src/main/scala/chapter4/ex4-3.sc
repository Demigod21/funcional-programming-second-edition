/**
 * Write a generic function map2 that combines two Option values using a biary function
 * If either Option value is None, then the return value is too
 * def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C]
 */

def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a,b) match
  case (Some(aa), Some(bb)) => Some(f(aa, bb))
  case _ => None