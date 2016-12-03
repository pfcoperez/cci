package org.pfcoperez.cci

object SortingAndSearching {

  // Exercise 10.2
  def anagramsTogether(words: Seq[String]): Seq[String] = words.sortBy(_.sorted)

}
