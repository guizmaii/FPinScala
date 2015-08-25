package chapter_2

import scala.annotation.tailrec

object PolymorphicFunction {

  @tailrec
  def isSorted[A](as: List[A])(ordered: (A, A) => Boolean): Boolean = as match {
    case Nil => true
    case a :: Nil => true
    case a :: b :: t => ordered(a, b) && isSorted(t)(ordered)
  }

  def main(args: Array[String]) {
    val sorted = List(1, 2, 3, 4, 5)
    println( isSorted(sorted)(_ < _) )
    println( isSorted(sorted)(_ > _) )
  }

}
