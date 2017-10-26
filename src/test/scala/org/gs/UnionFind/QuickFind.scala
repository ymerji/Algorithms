package org.gs.UnionFind

import org.scalatest.FlatSpec
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class QuickFindSuite extends FlatSpec{

  trait UnionFindBuilder {
    val tinyUFdata = Array((4,3),(3,8),(6,5),(9,4),(2,1),(5,0),(7,2),(6,1))
    val tinyUF = new QuickFind(10)
  }


  behavior of "a Union Find"

  it should "Make new commections and check is points are connected" in new UnionFindBuilder {
    for {
      t <- tinyUFdata
      if(!tinyUF.connected(t._1, t._2))
    } tinyUF.Union(t._1, t._2)
    assert(tinyUF.connected(4, 3) === true)
  }

}
