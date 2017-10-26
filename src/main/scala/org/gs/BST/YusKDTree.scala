package org.gs.BST

object YusKDTree {

  def apply[T](points: Seq[Seq[T]],depth:Int = 0)(implicit num:Numeric[T]):Option[YusKDNode[T]] = {
    if (points.isEmpty) None
    else {
      val axis = depth % 2
      val sorted = points.sortBy(_ (axis))
      val median = sorted(sorted.length / 2)(axis)
      val (left, right) = sorted.partition(p => num.lt(p(axis), median))
      Some(YusKDNode(right.head, apply(left, depth + 1), apply(right.tail, depth + 1), axis))

    }
  }

    case class YusKDNode[T](value: Seq[T],left: Option[YusKDNode[T]],right:Option[YusKDNode[T]],axis:Int)(implicit num:Numeric[T])
//    {
//      def nearest(to: Seq[T]): Nearest[T] ={
//
//      }
//
//
//    }
  }

object YusKDTreeTest extends App {

  val testpoints = List(List(2,3), List(5,4), List(9,6), List(4,7), List(8,1), List(7,2))
  val tree = YusKDTree(testpoints)
  println(tree)



}




