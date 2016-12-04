package org.pfcoperez.cci.treesandgraphs

object MinimalTree0402 extends App {

  trait BinaryTree[+T]

  case class Node[T](left: BinaryTree[T], v: T, right: BinaryTree[T]) extends BinaryTree[T]
  case object Empty extends BinaryTree[Nothing]

  def build[T](orderedUniques: Vector[T]): BinaryTree[T] =
    orderedUniques match {
      case Vector() => Empty
      case _ =>
        val n = orderedUniques.length
        val (leftPart, rightPart) = orderedUniques.splitAt(n/2)
        Node(
          build(leftPart),
          rightPart.head,
          build(rightPart.tail)
        )
    }

  val input = readLine.split(' ').map(_.toInt)

  val res = build(input.toSet.toVector.sorted)

  println(res)

}
