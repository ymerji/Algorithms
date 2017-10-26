package org.gs.BankOfAmerica


object ProblemA extends App{

  case class Point (
                     x:Double   ,
                     y:Double
                   )


  var loop:Boolean=true
  while (loop)
  {

    val b=readLine().split(' ').map(_.toDouble)

    if(b.length == 1)
      loop=false
    else
    {

      val pointA = Point(b(0),b(1))
      val pointB = Point(b(2),b(3))

      printf("%.10f", calculate(pointA,pointB,b(4)))

    }
  }


  def calculate( pointa:Point, pointb:Point,p:Double):Double = {

    val t = Math.abs(pointa.x-pointb.x)
    val c = Math.abs(pointa.y-pointb.y)

    Math.pow((Math.pow(t,p) +  Math.pow(c,p)),(1/p))
  }


}