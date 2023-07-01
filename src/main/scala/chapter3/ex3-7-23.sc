enum List[+A]:
  case Nil
  case Cons(head: A, tail: List[A])

object List:
  def apply[A](as: A*): List[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

def foldRight[A, B](as: List[A], acc: B, f: (A, B) => B): B =
  as match
    case Nil => acc
    case Cons(x, xs) => f(x, foldRight(xs, acc, f))

// foldRight is not specific to any type of element
// One way to descripte it is that it replaces the constructors of the list, Nil and Cons, with acc and f
// Cons(1, Cons(2, Nil))
// f   (1,    f(2, acc))

/**
 * 7.
 * Can product impelemented using foldRight, immediately halt the recursion and return 0.0 if it encounters a 0.0?
 * Why? Why not?
 * Consider how any short circuiting might work if you call foldRight with a large list.
 * This is a deeper question, which we'll return to in chapter 5
 */

// No, this is not possibile, foldRight recurses all the way to the end of the list before invoking the function

/**
 * 8.
 * See what happens when you pass Nil and Cons themselves to foldRight, like this:
 * foldRight(List(1,2,3), Nil:List[Int], Cons(_,_))
 * What do you think this says about relationship between foldRight and the data constructor List?
 *
 */

// foldRight(List(1,2,3), Nil:List[Int], Cons(_,_)) evaluates to
// Cons(1, Cons(2, Cons(3, Nil)))

/**
 * 9.
 *  Compute the length of a list using folRight
 *
 *  def length[A](as: List[A]): Int
 *
 *
 */

def length[A](as: List[A]): Int = foldRight(as, 0, (_, acc) => acc + 1)

/**
 * 10.
 *  Our implementation of foldRight is not tail recursive and will result in a stack overflow error for large list
 *  Convince yourself that this is the case, then write another general list recursion function, foldLeft, that
 *  is tailrecursive, using the technique we discussed in the previous chapter
 *  Start collapsing from the leftmost start of the list
 *
 *  def foldLeft[A, B](as: List[A], acc: B, f: (B, A) => B): B
 *
 *
 */

def foldLeft[A, B](as: List[A], acc: B, f: (B, A) => B): B = as match {
  case Nil => acc
  case Cons(hd, tl) => foldLeft(tl, f(acc, hd), f)
}
// If it's a cons we compute a new accumulator by calling f with the current accumulator and the head of the Cons
// After that, we recursively call foldLeft passing the tail and the new accumulator
// This is tail recursive because nothing extra is done after calling the recursive call

/**
 * 11.
 * Write sum, product, and a function to compute the length of a list using foldLeft

 */

def sum(ints: List[Int]): Int = foldLeft(ints, 0, (x, y) => x + y)

def product(ints: List[Double]): Double = foldLeft(ints, 1, (x, y) => x * y)

def newLength[A](as: List[A]): Int = foldLeft(as, 0, (acc, _) => acc + 1)

/**
 * 12.
 * Write a function that returns the reverse of a list
 * See if you can write it using a fold
 */

def reverse[A](as: List[A]): List[A] =
  foldLeft(as, Nil: List[A], (acc, a) => Cons(a, acc))

  // We use foldleft with an intial accumulator of an empty list
  // The combining function is Cons(a, acc)
  // Fold left walks through the elements left to right, the resulting list is built right to left, yielding a list in the reverse order

/**
 * 14.
 * Recall the signature of append:
 *
 * def append[A](a1: List[A], a2: List[A]): List[A]
 *
 * Implement append in terms of either foldLeft or foldRight instead of structural recursion
 *
 */

def append[A](a1: List[A], a2: List[A]): List[A] =
  foldRight(a1, a2, Cons(_,_))

/**
 * 15.
 * Write a function that concatenates a list of lists into a single list.
 * Its runtime shouldbe linear in the total lenght of all lists.
 * Try to use the functions we have already defined
 *
 *
 */

def concat[A](ll: List[List[A]]) : List[A] =
  foldRight(ll, Nil: List[A], append)

/**
 * 16.
 * Write a function that transforms a list of integers by adding 1 to each element (that is, given a list of integers,
 * it returns a new list of integers where each value is one more than the corresponding value in the original list)
 *
 *
 */

def incrementEach(l: List[Int]): List[Int] =
  foldRight(l, Nil: List[Int], (i, acc) => Cons(i + 1, acc))

//we use foldright so we can build the result in the correct order while using Cons as our combining function


/**
 * 17.
 * Write a function that turns each value in a List[Double] into a String
 * You can use the expression d.toString
 *
 */

def convertToString(as: List[Double]) : List[String] =
  foldRight(as, Nil: List[String], (d, acc) => Cons(d.toString, acc))


/**
 * 18.
 * Write a function, map, that generalizes modifying each element in a list while maintaining the structure of the list
 *
 * def map[A, B](as: List[A], f: A=> B): List[B]
 *
 */

def map[A, B](as: List[A], f: A => B): List[B] =
  foldRight(as, Nil: List[B], (a, acc) => Cons(f(a), acc))

// we use foldRight again, Cons as a combining function

/**
 * 19
 * Write a function, filter, that removes elements from a list unless they satisfy a given predicate
 * Use it to remove all odd numbers from a List[Int]
 *
 * def filter[A](as: List[A], f: A=> Boolean): List[A]
 *
 */

def filter[A](as: List[A], f: A => Boolean): List[A] =
  foldRight(as, Nil: List[A], (a, acc) => if f(a) then Cons(a, acc) else acc)

  // this is similiar to map, but we only create cons if the predicate is respected

/**
 * 20
 * Write a function, flatMap, that works like map except that the function given will return a list
 * instead of a single result, ensuring that the list is inserted into the final resulting list
 *
 * def flatMap[A, B](as: List[A], f: A => List[B]): List[B]
 *
 *
 */

def flatMap[A, B](as: List[A], f: A => List[B]): List[B] =
  foldRight(as, Nil: List[B], (a, acc) => append(f(a), acc))

  // we use foldRight, first converting A to a List[B
  // then we combine that with our accumulated List[B]

  //alternative

def flatMap2[A, B](as: List[A], f: A => List[B]): List[B] =
  concat(map(as, f))


/**
 * 21
 * Use flatmap to implement filter
 *
 */

def filterFlatMap[A](as: List[A], f: A => Boolean): List[A] =
  flatMap(as, a => if f(a) then List(a) else Nil)

/**
 * 22
 * Write a function that accept two lists and construct a new list by adding corresponding elements
 * List(1,2,3) and List(4,5,6) becomes List(5,7,9)
 *
 */

def addPair(a: List[Int], b: List[Int]) : List[Int] = (a, b) match
  case (Nil, _) => Nil
  case (_, Nil) => Nil
  case (Cons(hd1, tl1), Cons(hd2, tl2)) => Cons(hd1 + hd2, addPair(tl1, tl2))


/**
 * 23
 * Generalize the function you just wrote so it's not specific to integer or addition
 *
 */

def zipWith[A, B, C](a: List[A], b: List[B], f: (A, B) => C) : List[C] = (a, b) match
  case (Nil, _) => Nil
  case (_, Nil) => Nil
  case (Cons(hd1, tl1), Cons(hd2, tl2)) => Cons(f(hd1, hd2), zipWith(tl1, tl2, f))