package org.gs.BST

object KDTree {
  import Numeric._

  // Task 1A. Build tree of KDNodes. Translated from Wikipedia.
  def apply[T](points: Seq[Seq[T]], depth: Int = 0)(implicit num: Numeric[T]): Option[KDNode[T]] = {
    val dim = points.headOption.map(_.size) getOrElse 0
    if (points.isEmpty || dim < 1) None
    else {
      val axis = depth % dim
      val sorted = points.sortBy(_(axis))
      val median = sorted(sorted.size / 2)(axis)
      val (left, right) = sorted.partition(v => num.lt(v(axis), median))
      Some(KDNode(right.head, apply(left, depth + 1), apply(right.tail, depth + 1), axis))
    }
  }

  // Task 1B. Find the nearest node in this subtree. Translated from Wikipedia.
  case class KDNode[T](value: Seq[T], left: Option[KDNode[T]], right: Option[KDNode[T]], axis: Int)(implicit num: Numeric[T]) {
    def nearest(to: Seq[T]): Nearest[T] = {
      val default = Nearest(value, to, Set(this))
      compare(to, value) match {
        case 0 => default // exact match
        case t =>
          lazy val bestL = left.map(_ nearest to).getOrElse(default)
          lazy val bestR = right.map(_ nearest to).getOrElse(default)
          val branch1 = if (t < 0) bestL else bestR
          val best = if (num.lt(branch1.distsq, default.distsq)) branch1 else default
          val splitDist = num.minus(to(axis), value(axis))
          if (num.lt(num.times(splitDist, splitDist), best.distsq)) {
            val branch2 = if (t < 0) bestR else bestL
            val visited = branch2.visited ++ best.visited + this
            if (num.lt(branch2.distsq, best.distsq))
              branch2.copy(visited = visited)
            else best.copy(visited = visited)
          } else best.copy(visited = best.visited + this)
      }
    }
  }

  // Keep track of nodes visited, as per task. Pretty-printable.
  case class Nearest[T](value: Seq[T], to: Seq[T], visited: Set[KDNode[T]] = Set[KDNode[T]]())(implicit num: Numeric[T]) {
    lazy val distsq = KDTree.distsq(value, to)
    override def toString = f"Searched for=${to} found=${value} distance=${math.sqrt(num.toDouble(distsq))}%.4f visited=${visited.size}"
  }

  // Numeric utilities
  def distsq[T](a: Seq[T], b: Seq[T])(implicit num: Numeric[T]) =
  a.zip(b).map(c => num.times(num.minus(c._1, c._2), num.minus(c._1, c._2))).sum
  def compare[T](a: Seq[T], b: Seq[T])(implicit num: Numeric[T]): Int =
    a.zip(b).find(c => num.compare(c._1, c._2) != 0).map(c => num.compare(c._1, c._2)).getOrElse(0)
}

object KDTreeTest extends App {
  def test[T](haystack: Seq[Seq[T]], needles: Seq[T]*)(implicit num: Numeric[T]) = {
    println
    val tree = KDTree(haystack)
    if (haystack.size < 20) tree.foreach(println)
    for (kd <- tree; needle <- needles; nearest = kd nearest needle) {
      println(nearest)
      // Brute force proof
      val better = haystack
        .map(KDTree.Nearest(_, needle))
        .filter(n => num.lt(n.distsq, nearest.distsq))
        .sortBy(_.distsq)
      assert(better.isEmpty, s"Found ${better.size} closer than ${nearest.value} e.g. ${better.head}")
    }
  }

  // Results 1
  val wikitest = List(List(2,3), List(5,4), List(9,6), List(4,7), List(8,1), List(7,2))
  test(wikitest, List(9,2))

  // Results 2 (1000 points uniformly distributed in 3-d cube coordinates, sides 2 to 20)
  val uniform = for(x <- 1 to 10; y <- 1 to 10; z <- 1 to 10) yield List(x*2, y*2, z*2)
  assume(uniform.size == 1000)
  test(uniform, List(0, 0, 0), List(2, 2, 20), List(9, 10, 11))

  // Results 3 (1000 points randomly distributed in 3-d cube coordinates, sides -1.0 to 1.0)
  scala.util.Random.setSeed(0)
  def random(n: Int) = (1 to n).map(_ => (scala.util.Random.nextDouble - 0.5)* 2)
  test((1 to 1000).map(_ => random(3)), random(3))

  // Results 4 (27 points uniformly distributed in 3-d cube coordinates, sides 3...9)
  val small = for(x <- 1 to 3; y <- 1 to 3; z <- 1 to 3) yield List(x*3, y*3, z*3)
  assume(small.size == 27)
  test(small, List(0, 0, 0), List(4, 5, 6))
}