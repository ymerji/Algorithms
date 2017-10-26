package org.gs.UnionFind

/**
  * Created by yusuf on 17/07/2017.
  */
class QuickFind(n:Int) {

  private val zombies = new Array[Int](n)
  (0 to n-1).foreach(i => zombies(i) = i)
  println(zombies.mkString(" "))

  def connected(p:Int,q:Int) : Boolean = {
    zombies(p) == zombies(q)
  }

  def Union(p:Int,q:Int) : Unit ={

(0 to zombies.length-1).foreach(x => if(zombies(x) == p) zombies(x) = q )



  }


}

