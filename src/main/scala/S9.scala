import scala.annotation.tailrec

object S9 extends App {
  def pack[T](list: List[T]): List[List[T]] = {
    @tailrec
    def aux(currentList: List[T], list: List[T], acc: List[List[T]]) : List[List[T]] =
      list match {
        case Nil => currentList :: acc
        case x :: xs =>
          if (currentList.head == x)
            aux(x :: currentList, xs, acc)
          else
            aux(List(x), xs, currentList :: acc)
      }

    list match {
      case x :: xs => aux(List(x), xs, Nil).reverse
      case Nil => Nil
    }
  }

  val result = pack(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'))
  assert(result == List(List('a', 'a', 'a', 'a'), List('b'), List('c', 'c'), List('a', 'a'), List('d'), List('e', 'e', 'e', 'e')))
}
