package org.pfcoperez.cci.linkedlists

object PalindromeLinkedList0206 extends App {

  val input = readLine.split(' ').map(_.trim)

  def isPalindrome[T](l: LinkedList[T]): Boolean = l == l.reverse

  val Some(l) = LinkedList(input:_*)
  println(isPalindrome(l))


}
