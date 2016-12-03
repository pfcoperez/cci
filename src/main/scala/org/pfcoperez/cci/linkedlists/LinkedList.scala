package org.pfcoperez.cci.linkedlists


object LinkedList {
  def apply[T](input: T*): Option[LinkedList[T]] =
    (input :\ Option.empty[LinkedList[T]]) {
      (v, prev) => Some(LinkedList(v, prev))
    }
}

case class LinkedList[T](v: T, next: Option[LinkedList[T]]) {

  def reverse: LinkedList[T] = {
    def recReverse(scout: LinkedList[T], acc: Option[LinkedList[T]]): LinkedList[T] =
      scout.next map { next =>
        recReverse(next, Some(LinkedList(scout.v, acc)))
      } getOrElse scout.copy(next = acc)

    recReverse(this, None)
  }

}