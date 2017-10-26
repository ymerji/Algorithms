package org.gs.BST

import scala.collection.mutable.ArrayBuffer


object BSTApplication extends App {

  //Simple Range Search
  val alphabetList = scala.collection.mutable.TreeSet.empty[Char]
  alphabetList += 'B'
  alphabetList += 'D'
  alphabetList += 'A'
  alphabetList += 'I'
  alphabetList += 'H'
  alphabetList += 'F'
  alphabetList += 'P'
  println(alphabetList.range('G','K').mkString("-"))
  println("Count:" + alphabetList.range('G','K').count(c => true))

  //**********************************************************

  //  Line Segment Intersection - Horizontal Traversal Search - Sweep Line analysis

  val yCoordinates = scala.collection.mutable.TreeSet.empty[Int]
  val intersections = scala.collection.mutable.TreeSet.empty[(Int,Int)]
  case class Segment(pointA:(Int,Int),pointB:(Int,Int))


  val allSegments = ArrayBuffer[Segment]()
  allSegments += Segment((1,1),(3,1))
  allSegments += Segment((1,3),(3,3))
  allSegments += Segment((1,5),(3,5))
  allSegments += Segment((2,2),(2,6))

  val allSegments1 = Vector[Segment]() ++ allSegments

  val graphPoints = allSegments1.flatMap(s => List(s.pointA,s.pointB)).sortBy(_._1)

  graphPoints.foreach{p =>
    val elem = allSegments1.find(x => (x.pointA == p) || (x.pointB == p)).get
    if(elem.pointA._1 == elem.pointB._1){

      intersections ++= yCoordinates.range(elem.pointA._2,elem.pointB._2).map((p._1,_))

    }
    else if(yCoordinates.contains(p._2))
      {
        yCoordinates -= p._2
      }
    else yCoordinates += p._2

  }


  println(yCoordinates.mkString(","))













}

