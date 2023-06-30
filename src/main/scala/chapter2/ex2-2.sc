import scala.annotation.tailrec

/**
 * Implement isSorted, which checks whether an Array[A] is sorted
 * according to a given comparison function, gt, which returns true
 * if the first parameter is greater than the second parameter:
 *
 * def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean
 *
 */
def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
  
  @tailrec
  def loop(n: Int): Boolean = {
    if (n + 1 >= as.length) true
    else if (gt(as(n), as(n + 1))) false
    else loop (n + 1)
  }
  loop(0)

}