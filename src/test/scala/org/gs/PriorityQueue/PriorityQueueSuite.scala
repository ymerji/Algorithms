package org.gs.PriorityQueue
import org.scalatest.FlatSpec
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import scala.collection.mutable.ArrayBuffer

@RunWith(classOf[JUnitRunner])
class PriorityQueueSuite extends FlatSpec{

  trait PriorityQueueBuilder {
    val PQ = new PriorityQueue[Int](new ArrayBuffer(0))
  }

  behavior of "a Max PQ"

  it should "Insert new values and maintain the maximum at position 1" in new PriorityQueueBuilder {

    PQ.insert(2,PQ.greater)
    PQ.insert(3,PQ.greater)
    PQ.insert(1,PQ.greater)
    PQ.insert(7,PQ.greater)
    PQ.insert(8,PQ.greater)
    PQ.insert(50,PQ.greater)
    PQ.insert(99,PQ.greater)
    PQ.insert(10,PQ.greater)
    PQ.insert(17,PQ.greater)
    println(PQ.length)
    println(PQ.toString)
    assert(PQ.value === 99)
    PQ.pop(PQ.greater)
    assert(PQ.value === 50)
    PQ.pop(PQ.greater)
    assert(PQ.value === 17)
    PQ.pop(PQ.greater)
    assert(PQ.value === 10)

  }


  behavior of "a Min PQ"

  it should "Insert new values and maintain the Minimum at position 1" in new PriorityQueueBuilder {

    PQ.insert(2,PQ.less)
    PQ.insert(3,PQ.less)
    PQ.insert(1,PQ.less)
    PQ.insert(7,PQ.less)
    PQ.insert(8,PQ.less)
    PQ.insert(50,PQ.less)
    PQ.insert(99,PQ.less)
    PQ.insert(10,PQ.less)
    PQ.insert(17,PQ.less)
    println(PQ.length)
    println(PQ.toString)
    assert(PQ.value === 1)
    PQ.pop(PQ.less)
    assert(PQ.value === 50)
    PQ.pop(PQ.less)
    assert(PQ.value === 17)
    PQ.pop(PQ.less)
    assert(PQ.value === 10)

  }
}
