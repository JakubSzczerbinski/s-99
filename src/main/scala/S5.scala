import scala.annotation.tailrec

object S5 extends App {

  def reverse[T](list: List[T]): List[T] = {
    @tailrec
    def aux(list: List[T], acc: List[T]) : List[T] =
      list match {
        case x :: xs => aux(xs, x :: acc)
        case Nil => acc
      }
    aux(list, Nil)
  }

  assert(reverse(List(1, 2, 3)) == List(3, 2, 1))
}
