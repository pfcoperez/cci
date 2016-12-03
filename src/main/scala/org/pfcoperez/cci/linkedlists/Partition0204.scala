package org.pfcoperez.cci.linkedlists

object Partition0204 extends App {

  def partitionList[T](l: LinkedList[T])(x: T)(implicit ord: Ordering[T]): (Option[LinkedList[T]], Option[LinkedList[T]]) = {
    import ord.mkOrderingOps

    def recPartition(currentNode: LinkedList[T])(
      accA: Option[LinkedList[T]], accB: Option[LinkedList[T]]
    ): (Option[LinkedList[T]], Option[LinkedList[T]]) = {
      val LinkedList(v, continue) = currentNode
      val (newAccA, newAccB) =
        if(v <= x) (Some(LinkedList(v, accA)), accB)
        else (accA, Some(LinkedList(v, accB)))
      continue.map(next => recPartition(next)(newAccA, newAccB)) getOrElse (
        newAccA.map(_.reverse),
        newAccB.map(_.reverse)
      )
    }

    recPartition(l)(None, None)

  }

  val x = readInt
  val l = LinkedList((readLine.split(' ').map(_.trim.toInt)):_*)

  val (a, b) = partitionList(l.get)(x)

  println(a,b)

}
