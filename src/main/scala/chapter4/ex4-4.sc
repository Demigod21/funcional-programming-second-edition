/**
 * Write a function sequence that combines a list of Options into one Option containing
 * a list of all the Some values in the original list. If the original list contains None even once
 * the result of the funcitn should Be None, otherwiste, the result should be some with a listt of all values
 *
 * def sequence[A](as: List[Option[A]]): Option[List[A]]
 */

def sequence[A](as: List[Option[A]]): Option[List[A]] = {
  as.foldRight[Option[List[A]]](Some(Nil))((a, acc) => map2(a, acc)(_ :: _))
}