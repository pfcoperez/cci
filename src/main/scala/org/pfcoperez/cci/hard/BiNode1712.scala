package org.pfcoperez.cci.hard

object BiNode1712 {

  case class BiNode(data: Int, node1: Option[BiNode] = None, node2: Option[BiNode] = None) {
    override def toString(): String = s"${node1.map(_.data)}<-${data.toString}->${node2.map(_.data)}"
  }

  def toDoublyLinkedList(root: BiNode): BiNode = {
    def toDoublyLinkedList(root: BiNode, tail: Option[BiNode]): BiNode = {
      val rightSide = root.copy(node2 = root.node2.map(r => toDoublyLinkedList(r, tail)).orElse(tail))
      val leftSide = root.node1.map(l => toDoublyLinkedList(l, Some(rightSide)))
      leftSide.getOrElse(rightSide)
    }
    def updateLeftLinks(prev: Option[BiNode], scout: BiNode): BiNode = {
      scout.copy(node1 = prev, node2 = scout.node2.map(next => updateLeftLinks(Some(scout), next)))
    }
    updateLeftLinks(None, toDoublyLinkedList(root, None))
  }


  val D = BiNode(4)
  val E = BiNode(5)
  val F = BiNode(6)
  val G = BiNode(7)
  val B = BiNode(2, Some(D), Some(E))
  val C = BiNode(3, Some(F), Some(G))
  val A = BiNode(1, Some(B), Some(C))



}
