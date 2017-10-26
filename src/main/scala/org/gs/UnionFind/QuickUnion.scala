package org.gs.UnionFind

/**
  * Created by yusuf on 17/07/2017.
  */
class QuickUnion(n:Int) {

  val Id = new Array[Int] (n)

  (0 until n-1).foreach(i => Id(i) = 1)
  println(Id.mkString(" "))

  def connected(p:Int,q:Int) : Boolean = {
    Id(p) == Id(q)
  }

  def Union(p:Int,q:Int) = {
    Id(p) = Id(q)
  }

  private def root(i:Int):Int = i match{

    case x if(x == Id(x)) => x
    case x => root(Id(x))
  }

}
