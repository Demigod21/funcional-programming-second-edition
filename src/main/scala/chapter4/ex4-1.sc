/**
 * Implement all of the preceding functions on Option. As you impelemnt each function,
 * try to thing about what it means and in what situations you'd use it. We'll explore when
 * to use each of these functions next.
 * (option)
 * (map)
 * f(flatmap)
 * (getOrElse)
 * (orElse)
 * (filter)
 */

enum Option[+A] :
  case Some(get: A)
  case None

def map[B](f: A=> B): Option[B] = this match
  case Some(a) => Some(f(a))
  case None => None

/*
def flatMap[B](f: A => Option[B]): B = this match
  case Some(a) => f(a)
  case None => None
 */


def getOrElse[B>: A](default : => B): B = this match
  case Some(a) => a
  case None => default

def flatMap[B](f: A => Option[B]): B = map(f).getOrElse(None)


def orElse[B>: A](ob : => Option[B]): Option[B] = this match
  case Some(a) => Some(a)
  case None => ob


def filter(f: A => Boolean): Option[A] = flatMap( a=>
  if (f(a)) then Some(a)
  else None
)