package org.gs.BankOfAmerica



object ProblemB extends App{

  for(i <- 1 to 2)
  {

    val b=readLine().split(' ').map(_.toDouble)

    val totalGames:Int = b(0).toInt
    val probablity = b(1)

    //val x = List.fill(totalGames)(0 to 1).flatten.combinations(totalGames).flatMap(_.permutations).toList
    val p = (0 until (Math.pow(2,totalGames).toInt)).map(y => toBinary(y,totalGames)).toList

    val k = p.foldLeft(0.0)((x,y) => {val h = (longest(y)*prob(y,probablity))+x; "%.10f".format(h).toDouble})

    println(k)
  }


  def toBinary(i: Int, digits: Int) =
    String.format("%" + digits + "s", i.toBinaryString).replace(' ', '0')



  def prob (l:String,winProb:Double):Double = {

    val k = l.foldLeft(1.0)( (i,j) => j match {

      case '1' => (i*winProb)
      case '0' => i*(1-winProb)
    })

    "%.10f".format(k).toDouble

  }


  def longest(str:String):Int = {
    val pattern = "(1+)".r
    val m = (pattern findAllIn str).toList
    if (!(m isEmpty))
      m.maxBy(_.length).length
    else
      0
  }


}