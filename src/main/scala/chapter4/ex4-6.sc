/**
 * Implement Either with map, flatmap, orElse and map2
 */

enum Either[+E, +A]:
  case Left(value: E)
  case Right(value: A)

def map[B](f: A=> B): Either[E, B] = this match
  case Left(e) => Left(e)
  case Right(a) => Right(f(a))

def flatMap[EE >: E, B](f: A => Either[EE, B]) : Either[EE, B] = this match
  case Left(e) => Left(e)
  case Right(a) => f(a)

def orElse[EE >: E, B >: A](defaultValue: => Either[EE,B]) : Either[EE,B] = this match
  case Left(e) => defaultValue
  case Right(a) => Right(a)

def map2[EE >: E, B, C](that: Either[EE, B])(f: (A,B) => C) : Either[EE, C] =
  for{
    a <- this
    b <- that
  } yield f(a, b)
