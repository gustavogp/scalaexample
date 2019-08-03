package com.onprem

import scala.util.matching.Regex

object Chapter4 extends App{

  val numberPattern: Regex = "[0-9]".r
  numberPattern.findFirstMatchIn("password") match {
    case Some(_) => println("Password is good")
    case None => println("Password must contain a number")
  }

}
