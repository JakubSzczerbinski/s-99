import scala.annotation.tailrec

object S7 extends App {
  sealed trait ListTree[T]
  final case class Elem[T](t : T) extends ListTree[T]
  final case class Node[T](list : List[ListTree[T]]) extends ListTree[T]

  def flatten[T](tree: ListTree[T]): List[T] = {
    @tailrec
    def aux(tree: ListTree[T], nodeList: List[ListTree[T]], acc : List[T]): List[T] =
      (tree, nodeList) match {
        case (Elem(t), x :: xs) => aux(x, xs, t :: acc)
        case (Elem(t), Nil) => t :: acc
        case (Node(y :: ys), xs) => aux(Node(ys), y :: xs, acc)
        case (Node(Nil), x :: xs) => aux(x, xs, acc)
        case (Node(Nil), Nil) => acc
      }
    aux(tree, Nil, Nil)
  }

  val nestedList : ListTree[Int] =
    Node(List(
      Node(
        List(
          Elem(1),
          Elem(1)
        )
      ),
      Elem(2),
      Node(
        List(
          Elem(3),
          Node(
            List(
              Elem(5),
              Elem(8)
            )
          )
        )
      )
    ))
  assert(flatten(nestedList) == List(1, 1, 2, 3, 5, 8))

}
