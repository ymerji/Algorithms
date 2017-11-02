package org.gs.YusGraphs

import scala.collection.immutable.TreeMap


class DGraph {
  private var vertices = TreeMap.empty[Int,scala.collection.mutable.Set[Int]]

  def addEdge(v:Int,w:Int):Unit = {
    if(vertices.contains(v)) {
      val lstv = vertices(v) += w
      vertices += (v -> lstv)
    }
    else{
      vertices += (v -> scala.collection.mutable.Set(w))
    }

    if(!vertices.contains(w)) vertices += (w -> scala.collection.mutable.Set.empty[Int])
  }

  def adj(v:Int) : Seq[Int] = vertices(v).toSeq.sortWith(_ > _)

  def Vertices: Seq[Int] =  vertices.keys.toSeq
}


class DirectedDFS(g: DGraph, s: Int) {

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


class DirectedBFS(g: DGraph, s: Int) {
  import scala.collection.mutable.Queue
  var marked : Map[Int,(Option[Boolean],Option[Int],Option[Int])] = (g.Vertices zip Array.fill[(Option[Boolean],Option[Int],Option[Int])](g.Vertices.length)((Some(false),None,None))).toMap
  var vertices = Queue[Int]()
  marked += (s ->  (Some(true),None,Some(0)))
  vertices += s
  bfs(1)

  private def bfs(distanceTo : Int) : Unit = {
    var increaseDistance : Boolean = false
    if (vertices.isEmpty) return
    else {
      val v = vertices.dequeue()
      g.adj(v) foreach (w => if (!marked(w)._1.get) {
        vertices += w
        marked += (w -> (Some(true), Some(v), Some(distanceTo)))
        increaseDistance = true

      }
        )
    }
    if(increaseDistance) bfs(distanceTo + 1) else bfs(distanceTo)
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

}

class DirectedCC(g: DGraph) {

  private val connections = markConnections(0,(g.Vertices zip Array.fill[(Option[Boolean],Option[Int])](g.Vertices.length)((Some(false),None))).toMap)

  private def markConnections(count:Int,markedConnections:Map[Int,(Option[Boolean],Option[Int])]) : Map[Int,(Option[Boolean],Option[Int])]= {
    val firstUnmarked = markedConnections.find(x => x._2._1.get == false)
    if(firstUnmarked == None) markedConnections
    else {
      val dfs = new DirectedDFS(g, firstUnmarked.get._1)
      val result = markedConnections.map(x => if (dfs.marked(x._1)._1.get) (x._1 -> (Some(true), Some(count))) else x)
      markConnections(count + 1, result)
    }
  }

  def isConnected(v: Int,w:Int):Boolean = {
    connections(v)._2.get == connections(w)._2.get
  }

  def count : Int = {
    val totalConnections = connections.groupBy(x => x._2._2.get).size
    totalConnections
  }

  def id(v:Int) : Int = {
    connections(v)._2.get
  }
}


class DirectedCycle(g: DGraph) {

  var isCyclic = false

  var marked : Map[Int,Boolean] = (g.Vertices zip Array.fill[Boolean](g.Vertices.length)(false)).toMap

  marked.foreach(x => if (!x._2) dfs(x._1,x._1))

  private def dfs(child: Int,parent:Int): Unit = {
    g.adj(child) foreach (w =>
      if (!marked(w)) {
        marked += (w ->  true)
        dfs(w,child)
      }
      else if(w != parent) isCyclic = true

      )
  }

}

/// Depth first Order gives the tropological order. Diagraph can have topological order only if no directed cycles are present in the graph.

class depthFirstOrder(g: DGraph) {

  import scala.collection.mutable.Stack

  var reversePost = Stack[Int]()
  val marked: Map[Int, Boolean] = (g.Vertices zip Array.fill[Boolean](g.Vertices.length)(false)).toMap
  marked.foldLeft(marked) { (x, m) => RunDFSOnVertex(x, m._1) }

  def RunDFSOnVertex(vertices: Map[Int, Boolean], index: Int): Map[Int, Boolean] = {

    if (vertices(index) == true) vertices
    else {
      var markedVertices = vertices
      markedVertices += (index -> true)
      dfs(index)

      def dfs(child: Int): Unit = {
        g.adj(child) foreach (w =>
          if (!markedVertices(w)) {
            markedVertices += (w -> true
              )
            dfs(w)
          }
          )
        reversePost.push(child)
      }


      markedVertices

    }
  }
}




object DGraphTest extends App{

  val graph = new DGraph()
  graph.addEdge(4,2)
  graph.addEdge(2,3)
  graph.addEdge(3,2)
  graph.addEdge(6,0)
  graph.addEdge(0,1)
  graph.addEdge(2,0)
  graph.addEdge(11,12)
  graph.addEdge(12,9)
  graph.addEdge(9,10)
  graph.addEdge(9,11)
  graph.addEdge(8,9)
  graph.addEdge(10,12)
  graph.addEdge(11,4)
  graph.addEdge(4,3)
  graph.addEdge(3,5)
  graph.addEdge(6,8)
  graph.addEdge(8,6)
  graph.addEdge(5,4)
  graph.addEdge(0,5)
  graph.addEdge(6,4)
  graph.addEdge(6,9)
  graph.addEdge(7,6)

  graph.Vertices.foreach(v => println((v) + "--" + graph.adj(v).mkString(" ")))

  val dfs = new DirectedDFS(graph,0)

  dfs.marked.foreach(x => println(x._1 + "--" + x._2))
  dfs.hasPathTo(3)
  println(dfs.pathTo(3).mkString("-"))

  val graph1 = new DGraph()
  graph1.addEdge(5,0)
  graph1.addEdge(2,4)
  graph1.addEdge(3,2)
  graph1.addEdge(1,2)
  graph1.addEdge(0,1)
  graph1.addEdge(4,3)
  graph1.addEdge(3,5)
  graph1.addEdge(0,2)
  val bfs = new DirectedBFS(graph1,0)
  bfs.marked.foreach(x => println(x._1 + "--(" + x._2._1 + "," + x._2._2 +  "," +  x._2._3 + ")"))
  println(bfs.pathTo(3).mkString("-"))

  val cc = new DirectedCC(graph)
  println(cc.isConnected(2,7))
  println(cc.isConnected(5,3))
  println(cc.count)
  println(cc.id(7))

  val cyclic = new DirectedCycle(graph)
  println(cyclic.isCyclic)

  val noncyclicgraph = new DGraph()
  noncyclicgraph.addEdge(0,1)
  noncyclicgraph.addEdge(1,2)
  noncyclicgraph.addEdge(2,3)
  noncyclicgraph.addEdge(3,4)
  noncyclicgraph.addEdge(4,5)
  noncyclicgraph.addEdge(5,6)

  val noncyclic = new DirectedCycle(noncyclicgraph)
  println(noncyclic.isCyclic)



  val tropologicalOrderGraph = new DGraph()
  tropologicalOrderGraph.addEdge(0,5)
  tropologicalOrderGraph.addEdge(0,2)
  tropologicalOrderGraph.addEdge(0,1)
  tropologicalOrderGraph.addEdge(3,6)
  tropologicalOrderGraph.addEdge(3,5)
  tropologicalOrderGraph.addEdge(3,4)
  tropologicalOrderGraph.addEdge(5,2)
  tropologicalOrderGraph.addEdge(6,4)
  tropologicalOrderGraph.addEdge(6,0)
  tropologicalOrderGraph.addEdge(3,2)
  tropologicalOrderGraph.addEdge(1,4)

  val depthfirstorder = new depthFirstOrder(tropologicalOrderGraph)
  println(depthfirstorder.marked)








}
