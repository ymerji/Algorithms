package org.gs.sort

import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith


@RunWith(classOf[JUnitRunner])
class SelectionSortSuite extends  FlatSpec{

  trait SelectionSortBuilder{
  val inputList = List(30,-2,5,2,8,3,54,8,9,-90)
    val outputList = List(-90,-2,2,3,5,8,8,9,30,54)
  }

  behavior of "Selection Sort"

  it should "Sort the integer list" in new SelectionSortBuilder {
    val sortedArray = SelectionSort.ssort(inputList).reverse
    assert(sortedArray === outputList)
  }


}
