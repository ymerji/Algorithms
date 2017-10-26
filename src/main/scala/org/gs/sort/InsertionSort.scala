package org.gs.sort


class InsertionSort {

  def insert(x:Int, xs:List[Int]) : List[Int] = xs match {

      case List() => List(x)
      case y :: xs1 =>
        if (y >= x) x :: xs
        else y :: insert(x, xs)

    }

    def isort(xs: List[Int]): List[Int] = {
      xs match {
        case Nil => List()
        case x :: xs1 => insert(x, isort(xs1))
      }
    }





}
