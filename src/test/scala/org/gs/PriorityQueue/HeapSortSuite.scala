package org.gs.PriorityQueue

/**
  * Created by yusuf on 20/08/2017.
  */
import org.scalatest.FlatSpec
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import scala.collection.mutable.ArrayBuffer

@RunWith(classOf[JUnitRunner])
class HeapSortSuite extends FlatSpec{

  trait HeapSortBuilder {
    val PQ = new PriorityQueue[Int](new ArrayBuffer())
    val unsorted = new HeapSort[Int](ArrayBuffer(0,3,4,2,1,5))
  }

  behavior of "a Asending Heap Sort"

  it should "Sorting the given array into ascending order and returning a new array" in new HeapSortBuilder {

    val sortedArray = unsorted.sort(unsorted.greater)
    assert(sortedArray === ArrayBuffer(5,4,3,2,1))

  }


  behavior of "a Desceding Heap Sort"

  it should "Sorting the given array into desending order and returning a new array" in new HeapSortBuilder {

    val sortedArray = unsorted.sort(unsorted.less)
    assert(sortedArray === ArrayBuffer(1,2,3,4,5))

  }
}
