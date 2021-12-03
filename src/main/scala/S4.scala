import scala.annotation.tailrec

object S4 extends App {

  def length[T](list: List[T]): Int = {
    @tailrec
    def aux(list: List[T], acc: Int): Int =
      list match {
        case Nil => acc
        case _ :: xs => aux(xs, acc + 1)
      }
    aux(list, 0)
  }

  assert(length(List(1, 2, 3, 4, 5)) == 5)

}
