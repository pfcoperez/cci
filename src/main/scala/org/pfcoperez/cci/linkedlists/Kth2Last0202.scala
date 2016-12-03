package org.pfcoperez.cci.linkedlists

object Kth2Last0202 extends App {

  def kth2last[T](l: LinkedList[T])(k: Int): Option[LinkedList[T]] = {
    def recK2last(
                   scout: LinkedList[T],
                   ktolastRef: Option[LinkedList[T]],
                   n: Int): Option[LinkedList[T]] = {

      scout.next.map(next =>
        recK2last(next, if(n == k) Some(l) else ktolastRef.flatMap(_.next), n+1)
      ) getOrElse ktolastRef

    }
    recK2last(l, None, 0)
  }

  val l = (1 to 10).foldRight(Option.empty[LinkedList[Int]]) {
    case (v, prevList) => Some(LinkedList(v, prevList))
  }


  println(kth2last(l.head)(0))

}
