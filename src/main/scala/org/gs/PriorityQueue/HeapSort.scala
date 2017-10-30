package org.gs.PriorityQueue

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.PriorityQueue

/**
  * Created by yusuf on 20/08/2017.
  */
class HeapSort[A](pq:ArrayBuffer[A]) extends PriorityQueue(pq){

def sort(comp:(Int,Int)=> Boolean):ArrayBuffer[A]={

rearrangeOrder(comp)

val sortedArray = new ArrayBuffer[A]
   (1 to this.length).foreach(i => {
      sortedArray += this.pop(comp).get
    } )

  sortedArray
}

private def rearrangeOrder(comp:(Int,Int)=> Boolean):Unit={

  @tailrec
  def loop(k:Int): Unit ={
    if (k >= 1) {
      sink(k, comp)
      loop(k - 1)
    }
  }
   loop(this.length/2)
 }


}
