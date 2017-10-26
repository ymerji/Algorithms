package org.gs.UnionFind

/**
  * Created by yusuf on 17/07/2017.
  */
class WeightedPathCompressionQuickUnion(n :Int) {

  val Id = new Array[Int](n)
  (0 until n).foreach(i => Id(i) = i)
  print(Id.mkString(" "))



  def connected(p : Int, q :Int) : Boolean ={

    root(p) == root(q)
  }

  def Union(p :Int, q :Int) ={
    Id(p) = Id(q)
  }

  private def root(i:Int) :  Int = i match {

    case x if(x == Id(x)) => x
    case x => {
      Id(x) = root(Id(x))
      root(Id(x))
    }
  }


}
