package org.gs.PriorityQueue

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.math.Ordering

/**
  * Created by yusuf on 16/08/2017.
  */
class PriorityQueue[A](pq:ArrayBuffer[A]){

  if (pq.isEmpty) pq.append(null.asInstanceOf[A])
  else pq(0) = null.asInstanceOf[A]

  def less(a: Int, b: Int)(implicit ord: Ordering[A]): Boolean = ord.lt(pq(a), pq(b))

  def greater(a: Int, b: Int)(implicit ord: Ordering[A]): Boolean = ord.gt(pq(a), pq(b))

  def pop(comp:(Int,Int)=> Boolean): Option[A] ={

    val retValue = pq(1)
    swap(length,1)
    pq.remove(length)
    println(s"Queue Before sink ${this.toString}")
    if(this.length > 1)sink(1,comp)
    println(s"Queue after sink ${this.toString}")
    Some(retValue)
  }

  def length = pq.length - 1

  def insert(key : A,cmp:(Int,Int)=> Boolean ) ={
   // println(s"Queue Before Insert ${this.toString}")
    pq += key
    println(s"Queue Before Swim  ${this.toString}")
    swim(length,cmp)
    println(s"Queue After Swim ${this.toString}")
  }

  private def swim(index:Int,comp:(Int,Int)=> Boolean)={

    @tailrec
    def loop(i:Int,j:Int): Unit ={
      if(i > 1 && comp(i,j)) {
        swap(i, j)
        loop(j,j/2)
      }
    }
    loop(index,index/2)

  }

  protected def sink(k:Int,comp:(Int,Int)=> Boolean)={

    def loop(index:Int):Unit = {

      def calcJ(): Int = {
        val j = index * 2
        val j1 = j + 1
        //if ((j1 <= length) && comp(j, j1)) j else j1
        if (j1 > length)
          j
        else if (comp(j, j1))
          j
        else j1
      }
      val j = calcJ
      println(this.toString)
      if (comp(j,index)) {
        swap(index, j)
        if((j*2) + 1 <= this.length)loop(j)
      }
    }
    loop(k)
  }

  private def swap(child:Int,parent:Int)={
    val childValue = pq(child)
    val parentValue = pq(parent)
    pq.update(parent,childValue)
      //
    pq.update(child,parentValue)
  }



/*  def toString(keys: Seq[A]): String = {*/
   override def toString: String = {
    val sb = new StringBuilder()
    //keys foreach (s => if (s != null) sb append (s" $s"))
    pq foreach (s => if (s != null) sb append (s" $s"))
    sb.toString
  }

  def value : A = pq(1)

}


