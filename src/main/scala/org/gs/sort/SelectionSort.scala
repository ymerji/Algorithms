package org.gs.sort

import math.Ordering
import scala.annotation.tailrec

/** Divide list in 2 then sort each half, recursively
  *
  * implicitly uses math.Ordering
  *
  * @author Scala translation by Gary Struthers from Java by Robert Sedgewick and Kevin Wayne.
  *
  */
object SelectionSort {

  def ssort(xs:List[Int]) : List[Int] = {


    def loop(acc: List[Int], xs: List[Int]): List[Int] = xs match {
      case List() => acc
      case _ => loop((xs(xs.indexOf(xs.min)) :: acc), (xs.take(xs.indexOf(xs.min)) ::: xs.drop(xs.indexOf(xs.min) + 1)))
    }

    loop(List(),xs)
  }


}
