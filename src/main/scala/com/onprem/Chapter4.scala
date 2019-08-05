package com.onprem

//import scala.util.matching.Regex

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

}