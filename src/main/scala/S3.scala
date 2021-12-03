import scala.annotation.tailrec

object S3 extends App {
  @tailrec
  def nth[T](n: Int, list: List[T]): Option[T] =
    list match {
      case x :: _ if n == 0 => Some(x)
      case _ :: xs if n > 0 => nth(n - 1, xs)
      case _ if n < 0 => None
      case Nil => None
    }

  assert(nth(2, List(1, 1, 2, 3, 5, 8)) contains 2)
}
