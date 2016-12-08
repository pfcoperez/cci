package org.pfcoperez.cci.hard

object KthMultiple extends App {

  def kthMultiple(k: Int, primes: Set[Long]): Long = {
    import scala.collection.mutable.Set

    val multiples = Set.empty[Long]
    var scout: Long = 1
    var lastMultiple: Long = scout

    multiples.add(scout)
    multiples.add(1)

    while(multiples.size < k) {
      if(primes.exists(p => scout % p == 0 && multiples.contains(scout/p))) {
        multiples.add(scout)
        lastMultiple = scout
      }
      scout += 1
    }

    lastMultiple

  }

  (1 to 10) foreach { i =>
    println(kthMultiple(i, Set(3,5,7)))
  }

}
