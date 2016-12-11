package org.pfcoperez.cci.treesandgraphs

object FirstCommonAncestor extends App {

  trait BinaryTree[+T]
  case class Node[T](value: T, var leftFatherRight: (BinaryTree[T], BinaryTree[T], BinaryTree[T])) extends BinaryTree[T] {
    def setFather(node: Node[T]): Unit = {
      leftFatherRight = leftFatherRight.copy(_2 = node)
    }

    override def toString: String = value.toString
  }
  case object Empty extends BinaryTree[Nothing]

  def findCommonAncestor[T](a: BinaryTree[T], b: BinaryTree[T]): Option[Node[T]] = {
    def recFindCommon(
                       a: BinaryTree[T],
                       b: BinaryTree[T]
                     )(visited: Set[Node[T]]): Option[Node[T]] = (a, b) match {
      case (Empty, Empty) => None
      case (Empty, _: Node[T]) | (_: Node[T], Empty) =>
        val Seq(node @ Node(_, (_, father, _))) = Seq(a,b) collect {
          case node: Node[T] => node
        }
        if(visited contains node) Some(node)
        else recFindCommon(father, Empty)(visited)
      case (a: Node[T], b: Node[T]) =>
        if(visited contains a) Some(a)
        else if(visited contains b) Some(b)
        else if(a == b) Some(a)
        else {
          val Node(_, (_, aFather, _)) = a
          val Node(_, (_, bFather, _)) = b
          recFindCommon(aFather, bFather)(visited + a + b)
        }
    }

    recFindCommon(a, b)(Set.empty)

  }

  val D = Node[Int](4, (Empty, Empty, Empty))
  val E = Node[Int](5, (Empty, Empty, Empty))
  val C = Node[Int](3, (D,Empty,E))

  val B = Node[Int](2, (Empty, Empty, Empty))
  val A = Node[Int](1, (B, Empty, C))

  B.setFather(A)
  C.setFather(A)
  D.setFather(C)
  E.setFather(C)

  println(findCommonAncestor(D, E))

}
