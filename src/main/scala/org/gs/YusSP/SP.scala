package org.gs.YusSP


case class DirectedEdge(v: Int, w: Int, weight: Double)
//{

  //def compare(that: DirectedEdge): Int = weight.compareTo(that.weight)

  /** returns true if  other is an Edge */
 // def canEqual(other: Any): Boolean = other.isInstanceOf[DirectedEdge]

 // override def hashCode(): Int = 41 * (41 + v) + w + weight.hashCode

//  override def equals(that: Any): Boolean = that match {
//    case that: DirectedEdge => (that canEqual this) && this.hashCode == that.hashCode
//  }

//}

class EdgeWeightedDiGraph{

  private var vertices = Map.empty[Int,scala.collection.mutable.Set[DirectedEdge]]

  def addEdge(edge : DirectedEdge) : Unit ={

    if(vertices.contains(edge.v)) {
      val lstv = vertices(edge.v) += edge
      vertices += (edge.v -> lstv)
    }
    else{
      vertices += (edge.v -> scala.collection.mutable.Set(edge))
    }

    if(!vertices.contains(edge.w)) vertices += (edge.w -> scala.collection.mutable.Set.empty[Int])

  }

  def adj(v:Int) : Seq[DirectedEdge] = {
    vertices(v).toSeq
  }

  def V : Seq[Int] = vertices.keys.toSeq

  def edges : Seq[DirectedEdge] = {
    var edges = scala.collection.mutable.Set[DirectedEdge]()
    vertices.foreach(e => edges ++= e._2)
    edges.toSeq
  }
}



class DijkstraSP(g:EdgeWeightedDiGraph,source:Int){

  private var edgeTo  : Map[Int,Option[DirectedEdge]] = (g.V zip Array.fill[Option[DirectedEdge]](g.V.length)(None)).toMap
  private var DistanceTo :Map[Int,Double] = (g.V zip Array.fill[Double](g.V.length)(Double.PositiveInfinity)).toMap

  object MinOrder extends Ordering[(Int,Double)] {
    def compare(x:(Int,Double), y:(Int,Double)) = y._2 compare x._2
  }

 // private val pq = scala.collection.mutable.PriorityQueue.empty[(Int,Double)](MinOrder)
  private val pq = scala.collection.mutable.TreeSet.empty[(Int,Double)](MinOrder)

  DistanceTo += (source -> 0.0)
  pq += ((source,0.0))

  loop

  def loop: Unit = {
    if (pq.isEmpty) return
    val (v,w) = pq.firstKey                                      //repeatedly delete min weight edge from the queue
    pq.drop(0)
    g.adj(v).foreach(e => relax(e))
    loop
  }

  def relax(e:DirectedEdge):Unit = {

    val v = e.v
    val w = e.w
    val distToWBeforeRelaxing : Double = DistanceTo(w)
    if(distToWBeforeRelaxing > (DistanceTo(v) +  e.w) ) {
      DistanceTo += (w -> (DistanceTo(v) +  e.w))
      edgeTo += (w -> e)
      if (pq.exists(x => x._1 == w)) {
        pq -= ((w, distToWBeforeRelaxing))
        pq += ((w,DistanceTo(w)))
      }
         // pq += ((w,(DistanceTo(v) +  e.w)))
        // Unfortunately Scala's Prioriry Queue implementation does not support delete or Updating key.
       else                                             //Either use an impleneted version of PQ or a diffirent collection. else
       pq += ((w,(DistanceTo(v) +  e.w)))
    }

  }


  def distTo(v:Int): Double = {

    2.00
  }

  def pathTo(v:Int): Seq[DirectedEdge] = {

    Seq(new DirectedEdge(1,2,3))

  }

  def hasPathTo(v:Int) : Boolean = {

    true

  }

}




object SP extends App{

}
