import scala.annotation.tailrec

object S1 extends App {
  def fold_last[T](list : List[T]) : Option[T] =
    list.foldRight[Option[T]](None) {
      (el, result) => result orElse Some(el)
    }
  @tailrec
  def rec_last[T](list: List[T]): Option[T] =
    list match {
      case Nil => None
      case x :: Nil => Some(x)
      case _ :: xs => rec_last(xs)
    }

  val list = List(1, 2, 3, 4, 6)
  assert(fold_last(list) contains 6)
  assert(rec_last(list) contains 6)
}
