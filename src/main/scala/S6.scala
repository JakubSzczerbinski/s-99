import scala.annotation.tailrec

object S6 extends App {
  def isPalindrome[T](list: List[T]): Boolean = {
    @tailrec
    def aux(list: List[T], rev: List[T], it: List[T]): Boolean = {
      (list, it) match {
        case (x :: xs, _ :: _ :: ys) =>
          aux(xs, x :: rev, ys)
        case (_ :: xs, _ :: Nil) =>
          xs == rev
        case (xs, Nil) =>
          xs == rev
      }
    }
    aux(list, Nil, list)
  }

  assert(isPalindrome(List(1, 2, 2, 1)))
  assert(isPalindrome(List(1, 2, 3, 2, 1)))
  assert(!isPalindrome(List(1, 1, 3, 2, 1)))
  assert(!isPalindrome(List(1, 1, 2, 1)))
}
