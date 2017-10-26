package org.gs.ThreeSum


trait SumToZero{

  @throws(classOf[SumToZeroNotFound])
  def process(arr: Array[Int], size: Int = 3): Array[Int]
}


object SumToZeroBruteForce extends SumToZero{
  //brute force process any size combination
  override def process(arr: Array[Int], size: Int = 3): Array[Int] = {
    arr.combinations(size).find(_.sum == 0).getOrElse(throw new SumToZeroNotFound)
  }
}

class SumToZeroNotFound(msg:String=null, cause:Throwable=null)
  extends java.lang.Exception (msg, cause) {}


object SumToZeroBinSearch extends SumToZero{
  //optimized with sorting and binary search

  //scala does not seem to have a native binary search routine
  implicit private class Search(val arr: Array[Int]){
    def binSearch(target: Int) = {
      java.util.Arrays.binarySearch(arr.asInstanceOf[Array[Int]], target)
    }
  }

  @throws(classOf[IllegalArgumentException])
  override def process(arr: Array[Int], size: Int = 3): Array[Int] = size match {
    case 3 =>
      val sorted_arr = arr.sortWith(_<_)
      val max_len = sorted_arr.length
      for((i_val, i) <- sorted_arr.takeWhile(_ <= 0).zipWithIndex ){
        val maxj = sorted_arr(max_len - 1) - i_val
        for(j_val <- sorted_arr.slice(i+1, max_len).takeWhile(_ <= maxj)){
          val temp_sum = i_val + j_val
          val res_idx = sorted_arr.binSearch(-temp_sum)
          if (res_idx > -1) {
            return Array(i_val, j_val, sorted_arr(res_idx))
          }
        }
      }
      throw new SumToZeroNotFound
    case _ => throw new IllegalArgumentException("only support size of three for now")
  }
}

object SumToZero{

  def apply(impl: String) = impl match{
    case "with_bin_search" => SumToZeroBinSearch
    case _ => SumToZeroBruteForce
  }
}