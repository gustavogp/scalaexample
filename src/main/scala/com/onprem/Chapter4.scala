package com.onprem

//import scala.util.matching.Regex
import scala.collection.immutable.List

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match{
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match{
    case None => default
    case Some(a) => a
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match{
    case None => ob
    case Some(a) => Some(a)
  }

  def filter(f: A => Boolean): Option[A] = this match{
    case Some(a) if f(a) => this
    case _ => None
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]


// from scala web tutorial
/*object MyRegex extends App{

  val numberPattern: Regex = "[0-9]".r
  numberPattern.findFirstMatchIn("password") match {
    case Some(_) => println("Password is good")
    case None => println("Password must contain a number")
  }
}
*/

object Chapter4 extends App{
  val num: Option[Int] = Some(100)
  val mult = num.map(_*2)
  println(mult.orElse(Some(-1)))

  //ex. 4 my implementation
  def mean(list: scala.collection.immutable.List[Float]): scala.collection.immutable.List[Float] = {
    scala.collection.immutable.List(list.sum / list.length)
  }

  def variance(xs:scala.collection.immutable.List[Float]): Option[Float] = {
   Some(mean(xs).flatMap(m => mean(xs.map((x) => math.pow(x-m,2).toFloat))).head)
  }

  //ex 4 books github implementation
  def mean2(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance2(xs: Seq[Double]): Option[Double] =
    mean2(xs) flatMap (m => mean2(xs.map(x => math.pow(x - m, 2))))

  // println(mean(scala.collection.immutable.List(1,2,3,6)))
  println(variance(scala.collection.immutable.List(1f,2,3,6)).getOrElse(-1.0))
  println(variance2(scala.collection.immutable.List(1d,2,3,6)).getOrElse(-1.0))
}
