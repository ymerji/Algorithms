package org.gs.ThreeSum

import org.junit.runner.RunWith
import org.scalatest.junit._
import org.scalatest._
import scala.runtime.ScalaRunTime.stringOf


@RunWith(classOf[JUnitRunner])
class TestSumToZero extends FunSuite{

  final val methods = Array("brute_force", "with_bin_search")

  def testAllMethods(arr: Array[Int], expected_res: String = "",
                     methods: Array[String] = this.methods) = {
    for (method <- methods){
      println("testing: " + method)
      val stz = SumToZero(method)
      val res = stz.process(arr)
      val str_res = stringOf(res.sortWith(_<_))
      assert( str_res == expected_res,
        "res: " + str_res)
    }
  }

  test("should return entire array that sums to zero") {
    testAllMethods(Array(1,2,-3), "Array(-3, 1, 2)")
  }

  test("should raise NotFound exception if no combination sums to zero"){
    this.methods.foreach(method =>
      intercept[SumToZeroNotFound]{
        testAllMethods(Array(1,2,3), "", Array(method))
      })
  }
  test("should return first set of nums that sum to zero"){
    testAllMethods(Array(125, 124, -100, -25, 1, 2, -3, 10, 100), "Array(-100, -25, 125)")
  }


}