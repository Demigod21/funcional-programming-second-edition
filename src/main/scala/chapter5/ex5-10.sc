/**
 * Write a function fibs that generates the infite lazy list of fibonacci numbers
 */

def fibonacci(): MyList[Int] = {

  def loop(current: Int, next: Int): MyList[Int] =
    cons(current, go(next, current + next))

    loop(0, 1)

}