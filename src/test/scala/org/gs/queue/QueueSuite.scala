package org.gs.queue
/** @see http://algs4.cs.princeton.edu/13stacks/tobe.txt
  */
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.gs.fixtures.WordArrayBuilder
import scala.collection.mutable.ArrayBuffer

/** @author Gary Struthers
  *
  */
@RunWith(classOf[JUnitRunner])
class QueueSuite extends FlatSpec {
  behavior of "a Queue"
  it should "enqueue and dequeue characters in LIFO order" in {
    val q = Queue('P', 'Q', 'E')
    val q1 = q enqueue 'Z'
    assert(q.head === 'P')
    assert(q.tail.head === 'Q')
    val qt = q.tail
    assert(qt.head === 'Q')
    val qtt = qt.tail
    assert(qtt.head === 'E')
    val qttf = qtt.tail
    intercept[NoSuchElementException](qttf.head)
    val qz = q1.tail.tail.tail.head
    assert(qz === 'Z')
  }

  it should "queue and dequeue words" in new WordArrayBuilder {
    val managedResource = readURI("http://algs4.cs.princeton.edu/13stacks/tobe.txt")
    val strings = managedResource.loan(readFileToArray).toArray
    val q = Queue(strings:_*)
    def dequeueAll(xs: Queue[String], buff: ArrayBuffer[String]): Array[String] = {
      if(!xs.isEmpty) dequeueAll(xs.tail, buff += xs.head) else buff.toArray
    }
    val lifo = dequeueAll(q, new ArrayBuffer[String]())
    assert(lifo === strings, s"$lifo not equal ${strings mkString ","}")
  }
}
