package org.pfcoperez.cci.treesandgraphs

object CheckSubtree0410 extends App {

  trait BinaryTree[+T]
  case class Node[T](left: BinaryTree[T], v: T, right: BinaryTree[T]) extends BinaryTree[T]
  case object Empty extends BinaryTree[Nothing]

  def isSubtree[T](t: BinaryTree[T])(st: BinaryTree[T]): Boolean = {
    (t, st) match {
      case (Empty, Empty) => true
      case (Node(tLeft, tV, tRight), Node(sLeft, sV, sRight)) if (tV == sV) =>
        isSubtree(tLeft)(sLeft) && isSubtree(tRight)(sRight)
      case (Node(tLeft, _, tRight), _) =>
        isSubtree(tLeft)(st) || isSubtree(tRight)(st)
      case _ => false
    }
  }

}
