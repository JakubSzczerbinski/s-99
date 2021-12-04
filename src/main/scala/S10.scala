import scala.annotation.tailrec

object S10 extends App {
  def encode[T](list: List[T]) : List[(T, Int)] = {
    @tailrec
    def aux(list: List[T], acc: List[(T, Int)]): List[(T, Int)] =
      list match {
        case Nil => acc.reverse
        case x :: xs =>
          acc match {
            case (el -> count) :: ys if el == x =>
              aux(xs, el -> (count + 1) :: ys)
            case _ => aux(xs, x -> 1 :: acc)
          }
      }
    aux(list, Nil)
  }

  val result = encode(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'))
  assert(result == List('a' -> 4, 'b' -> 1, 'c' -> 2, 'a' -> 2, 'd' -> 1, 'e' -> 4))
}
