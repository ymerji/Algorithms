package org.gs.UnionFind

/**
  * Created by yusuf on 17/07/2017.
  */
class QuickUnion(n:Int) {

  val Id = new Array[Int] (n)

  (0 until n).foreach(i => Id(i) = i)
  println(Id.mkString(" "))

  def connected(p:Int,q:Int) : Boolean = {
    Id(p) == Id(q)
  }

  def union(p:Int,q:Int) = {
    Id(q) = root(p)
  }

  private def root(i:Int):Int = i match{

    case x if(x == Id(x)) => x
    case x => root(Id(x))
  }

}

object QuickUnion extends App{

  val qu = new QuickUnion(10)
  qu.union(4,3)
  qu.union(3,8)
  qu.union(6,5)
  qu.union(9,4)
  qu.union(2,1)
  println(qu.connected(8,9))
  println(qu.connected(5,4))
}
