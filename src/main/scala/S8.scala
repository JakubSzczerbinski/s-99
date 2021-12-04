import scala.annotation.tailrec

object S8 extends App {
  def compress[T](list: List[T]) : List[T] = {
    @tailrec
    def aux(lastEl: Option[T], list: List[T], acc: List[T]): List[T] =
      list match {
        case x :: xs if lastEl contains x => aux(Some(x), xs, acc)
        case x :: xs => aux(Some(x), xs, x :: acc)
        case Nil => acc
      }
    aux(None, list, Nil).reverse
  }

  val result = compress(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'))
  assert(result == List('a', 'b', 'c', 'a', 'd', 'e'))
}
