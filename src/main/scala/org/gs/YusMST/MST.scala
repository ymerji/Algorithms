package org.gs.YusMST

import org.gs.UnionFind.QuickUnion
import org.gs.set.UF

/**
  * Created by yusuf on 04/11/2017.
  */
case class Edge(v: Int, w: Int, weight: Double) extends Ordered[Edge] {

  def either(): Int = v

  /** returns the matchinf "from" or "to" vertex for  vertex, error if no match */
  def other(vertex: Int): Int = vertex match {
    case `v` => w
    case `w` => v
    case _ => throw new IllegalArgumentException(s"Illegal endpoint vertex:${vertex}")
  }

  /** returns -1 if this < that, 0 if equal, +1 if > */
  def compare(that: Edge): Int = weight.compareTo(that.weight)

  /** returns true if  other is an Edge */
  def canEqual(other: Any): Boolean = other.isInstanceOf[Edge]

  override def hashCode(): Int = 41 * (41 + v) + w + weight.hashCode

  override def equals(that: Any): Boolean = that match {
    case that: Edge => (that canEqual this) && this.hashCode == that.hashCode
  }

}

class EdgeWeightedGraph{

  private var vertices = Map.empty[Int,scala.collection.mutable.Set[Edge]]

  def addEdge(edge : Edge) : Unit ={

    val v  = edge.either()
    val w = edge.other(v)

    if(vertices.contains(v)) {
      val lstv = vertices(v) += edge
      vertices += (v -> lstv)
    }
    else{
      vertices += (v -> scala.collection.mutable.Set(edge))
    }
    if(vertices.contains(w)) {
      val lstw = vertices(w) += edge
      vertices += (w -> lstw)
    }
    else{
      vertices += (w -> scala.collection.mutable.Set(edge))
    }
  }

  def adj(v:Int) : Seq[Edge] = {
    vertices(v).toSeq
  }

  def V : Seq[Int] = vertices.keys.toSeq

  def edges : Seq[Edge] = {
    var edges = scala.collection.mutable.Set[Edge]()
    vertices.foreach(e => edges ++= e._2)
    edges.toSeq
  }
}



class KruskalMST(g : EdgeWeightedGraph) {

  object MinOrder extends Ordering[Edge] {
    def compare(x:Edge, y:Edge) = y compare x
  }
  val mst = scala.collection.mutable.Queue.empty[Edge]

  val pq = scala.collection.mutable.PriorityQueue.empty[Edge](MinOrder)

  g.edges.foreach(e => pq.enqueue(e))

   //val uf = new QuickUnion(g.V.length)
  val uf =  new UF(g.V.length)

  (0 until pq.length).foreach(i=>
  {
    val edge = pq.dequeue()
    val v = edge.either()
    val w = edge.other(v)
    if(!uf.connected(v,w))
      {
                uf.union(v,w)
                mst.enqueue(edge)
      }
  })

  def edges : Seq[Edge] = {

    mst
}

  def weight : Double = {
    2.00
  }
}

class PrimsMST(g : EdgeWeightedGraph) {      // Assume g graph is connected

  object MinOrder extends Ordering[Edge] {
    def compare(x:Edge, y:Edge) = y compare x
  }
  val mst = scala.collection.mutable.Queue.empty[Edge]
  val pq = scala.collection.mutable.PriorityQueue.empty[Edge](MinOrder)
  val marked =  Array.fill[Boolean](g.V.length)(false)
  visit(0)    // Start from vertex 0 and mark it as visited and add all its adjascent edges to PQ providd vertedx is not marked.
  loop

  def loop: Unit = {
    if (pq.isEmpty) return
    val e = pq.dequeue()                                        //repeatedly delete min weight edge from the queue
    val v = e.either()
    val w = e.other(v)
    if (marked(v) && marked(w)) loop else mst.enqueue(e)      // Ignore if both end pointes from marked. else Add the edge to mst tree
    if(!marked(v)) visit(v)
    if(!marked(w)) visit(w)
    loop
  }

  def visit(v:Int):Unit = {
    marked(v) = true
    g.adj(v).foreach(e => if(!marked(e.other(v))) pq.enqueue(e))
  }

  def edges : Seq[Edge] = {
    mst
  }

  def weight : Double = {
    2.00
  }
}

object MSTTest extends App{
  val weightedGraph = new EdgeWeightedGraph()
  weightedGraph.addEdge(Edge(6,2,0.40))
  weightedGraph.addEdge(Edge(3,6,0.52))
  weightedGraph.addEdge(Edge(0,7,0.16))
  weightedGraph.addEdge(Edge(2,3,0.17))
  weightedGraph.addEdge(Edge(6,0,0.58))
  weightedGraph.addEdge(Edge(6,4,0.93))
  weightedGraph.addEdge(Edge(1,7,0.19))
  weightedGraph.addEdge(Edge(0,2,0.26))
  weightedGraph.addEdge(Edge(5,7,0.28))
  weightedGraph.addEdge(Edge(1,3,0.29))
  weightedGraph.addEdge(Edge(3,6,0.52))
  weightedGraph.addEdge(Edge(1,5,0.32))
  weightedGraph.addEdge(Edge(2,7,0.34))
  weightedGraph.addEdge(Edge(4,5,0.35))
  weightedGraph.addEdge(Edge(1,2,0.36))
  weightedGraph.addEdge(Edge(4,7,0.37))
  weightedGraph.addEdge(Edge(0,4,0.38))
  weightedGraph.addEdge(Edge(6,2,0.40))
  weightedGraph.addEdge(Edge(3,6,0.52))
  val kuskalMST = new KruskalMST(weightedGraph)
  println(kuskalMST.edges.mkString("---"))
  val primsMST = new PrimsMST(weightedGraph)
  println(primsMST.edges.mkString("---"))
}

