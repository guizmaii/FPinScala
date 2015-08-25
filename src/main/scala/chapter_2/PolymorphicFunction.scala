package chapter_2

import scala.annotation.tailrec

object PolymorphicFunction {

  // Ex 2.2
  @tailrec
  def isSorted[A](as: List[A])(ordered: (A, A) => Boolean): Boolean = as match {
    case Nil => true
    case a :: Nil => true
    case a :: b :: t => ordered(a, b) && isSorted(t)(ordered)
  }

  // Ex 2.3
  def curry[A, B, C](f: (A, B) => C): A => B => C = a => b => f(a, b)

  // Ex 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  // Ex 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))
  def compose2[A, B, C](f: B => C, g: A => B): A => C = g andThen f

  def main(args: Array[String]) {
    val sorted = List(1, 2, 3, 4, 5)
    println( isSorted(sorted)(_ < _) )
    println( isSorted(sorted)(_ > _) )

    val curried: Int => Int => Boolean = curry[Int, Int, Boolean](_ < _)
  }

}
