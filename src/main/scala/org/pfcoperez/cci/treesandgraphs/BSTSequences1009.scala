package org.pfcoperez.cci.treesandgraphs

object BSTSequences1009 extends App {

  trait BinaryTree[+T]
  case class Node[+T](v: T, left: BinaryTree[T] = Empty, right: BinaryTree[T] = Empty) extends BinaryTree[T]
  case object Empty extends BinaryTree[Nothing]

  def generators[T](btree: BinaryTree[T], prefix: Vector[T] = Vector.empty): Seq[Vector[T]] =
    btree match {
      case Empty => Seq(prefix)
      case Node(v, left, Empty) =>
        generators(left, prefix :+ v)
      case Node(v, Empty, right) =>
        generators(right, prefix :+ v)
      case Node(v, left, right) =>
        (generators(left, prefix :+ v) flatMap { leftFirstGenerator =>
          generators(right, leftFirstGenerator)
        }) ++ (generators(right, prefix :+ v) flatMap { rightFirstGenerator =>
          generators(left, rightFirstGenerator)
        })
  }

  val treeA = Empty
  val treeB = Node(2, Node(1), Node(3))
  val treeC = Node(7, Node(1, right = Node(2, right = Node(4))))
  val treeD = Node(5, Node(3, Node(2, Node(1)), Node(4)), Node(7, Node(6), Node(8)))

  val cases = List(
    "A" -> treeA,
    "B" -> treeB,
    "C" -> treeC,
    "D" -> treeD
  )

  for((name, example) <- cases) {
    val title = s"\nGenerators for case $name"
    println(title)
    println("-"*title.size)
    println
    generators(example) foreach { generator =>
      println(s"\t$generator")
    }
  }

}
