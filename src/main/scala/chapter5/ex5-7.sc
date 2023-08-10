/**
 * Implement map filter append and flatmap using folridhgt
 * The append method should be nonstrict in its argument
 */

def map[B](f: A => B): MyList[B] =
  foldRight(empty[B])((a, b) => cons(f(a), b)

def filter(p: A => Boolean): MyList[A] =
  foldRight(empty[B])((a, b) => if p(a) then cons(a, b) else b)

def append[A2 >: A](that: => MyList[A2]): MyList[A2] =
  foldRight(that)((a, b) => cons(a, b))

def flatMap[B](f: A => MyList[B]) : MyList[B] =
  foldRight(empty[B])((a, b) => f(a).append(b)
