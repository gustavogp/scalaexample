package com.onprem



sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }


  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  //exercise 3
  def setHead[A](list:List[A], h:A):List[A] = list match {
    case Nil => sys.error("tail of empty list")
    case Cons(_,t) => Cons(h, list)
  }

  //exercise 4
  def drop[A](l:List[A], n:Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_,t) => drop(t, n-1)
    }

  //exercise 5
  def dropWhile[A](l:List[A])(f:A => Boolean):List[A] =
    l match {
      case Cons(h,t) if f(h) => dropWhile(t)(f)
      case _ => l
    }

  //exercise 6
  def init[A](l:List[A]): List[A] =
    l match {
      case Nil => sys.error("init of empty list")
      case Cons(_,Nil) => Nil
      case Cons(h,t) => Cons(h,init(t))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)


}

//exercise 1
object App extends App {
  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(4,_)) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4,_)))) => x + y
    case Cons(h,t) => h + List.sum(t)
    case _ => 101
  }
 // println( x )
//  println(List.setHead(List(2,3,4), 1))
//  println(List.drop(List(1,2,3,4,5), 3))
//  println(List.dropWhile(List(1,2,3)) (x => x == 1))
//  println(List.init(List(1,2,3,4)))
  println(List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)))
  println(List.foldRight(List(1,2,3,4,5),1)(_*_))
}

//exercise 2: it's a constant time because it always returns a reference to the first element of the tail, regardless of how long the list is

//exercise 4


