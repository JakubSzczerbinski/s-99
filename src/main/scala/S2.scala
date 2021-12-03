import scala.annotation.tailrec

object S2 extends App {
  @tailrec
  def rec_penultimate[T](list: List[T]): Option[T] =
    list match {
      case _ :: Nil | Nil => None
      case x :: _ :: Nil => Some(x)
      case _ :: xs => rec_penultimate(xs)
    }

  assert(rec_penultimate(List(1, 4, 5)) contains 4)
}
