package org.gs.YusGraphs

import scala.collection.immutable.TreeMap


class UGraph {
  private var vertices = TreeMap.empty[Int,scala.collection.mutable.Set[Int]]
  def addEdge(v:Int,w:Int):Unit = {
    if(vertices.contains(v)) {
      val lstv = vertices(v) += w
      vertices += (v -> lstv)
    }
    else{
      vertices += (v -> scala.collection.mutable.Set(w))
    }
    if(vertices.contains(w)) {
      val lstw = vertices(w) += v
      vertices += (w -> lstw)
    }
    else{
      vertices += (w -> scala.collection.mutable.Set(v))
    }
  }
  def adj(v:Int) : Seq[Int] = vertices(v).toSeq.sortWith(_ > _)

  def Vertices: Seq[Int] =  vertices.keys.toSeq
}


class DFS(g: UGraph, s: Int) {

  var marked : Map[Int,(Option[Boolean],Option[Int])] = (g.Vertices zip Array.fill[(Option[Boolean],Option[Int])](g.Vertices.length)((Some(false),None))).toMap

  private def dfs(v: Int): Unit = {
    g.adj(v) foreach (w => if (!marked(w)._1.get) {
      marked += (w ->  (Some(true),Some(v)))
      dfs(w)}
      )
  }
  def hasPathTo(v :Int) : Boolean = {

    marked(v)._1.get
  }

  def pathTo(v:Int) : Seq[Int] = {

    val stack = new scala.collection.mutable.Stack[Int]
    if(!hasPathTo(v)) Seq.empty[Int]
    else
      {
        stack.push(v)
        def pushOnStack(index :Int) : Unit= {
          if(index == s) return
          val edgeToValue = marked(index)._2.get
          stack.push(edgeToValue)
          pushOnStack(edgeToValue)
        }
        pushOnStack(v)
        stack
      }

  }

  marked += (s ->  (Some(true),None))
  dfs(s)


}



object UGraphTest extends App{

  val graph = new UGraph()
  graph.addEdge(0,5)
  graph.addEdge(4,3)
  graph.addEdge(0,1)
  graph.addEdge(9,12)
  graph.addEdge(6,4)
  graph.addEdge(5,4)
  graph.addEdge(0,2)
  graph.addEdge(11,12)
  graph.addEdge(9,10)
  graph.addEdge(0,6)
  graph.addEdge(7,8)
  graph.addEdge(9,11)
  graph.addEdge(5,3)
  graph.Vertices.foreach(v => println((v) + "--" + graph.adj(v).mkString(" ")))

  val dfs = new DFS(graph,0)

  dfs.marked.foreach(x => println(x._1 + "--" + x._2))
  dfs.hasPathTo(3)
  println(dfs.pathTo(3).mkString("-"))

}
