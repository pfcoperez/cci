package org.pfcoperez.cci.hard

object WordTransformer1722 extends App {

  implicit class StringWithUpdate(str: String) {
    def update(idx: Int, v: Char): String = {
      val (a,b) = str.splitAt(idx)
      s"$a$v${b.tail}"
    }
  }

  def transformWord(goal: String, dictionary: Set[String])(path: List[String]): Option[List[String]] =
    if(path.head == goal) Some(path)
    else {
      require(from.size == to.size, "original and goal word must have the same length")
      val solutions = (path.head zip goal.zipWithIndex) flatMap {
        case (sc, (gc, idx)) if sc != gc =>
          val start = path.head.updated(idx, gc)
          if(dictionary contains start) transformWord(goal, dictionary)(start::path).toSeq
          else Seq()
        case _ => Seq()
      }
      solutions.headOption map { _ =>
        solutions.minBy(_.size)
      }
    }


  val Array(from, to) = readLine.split(' ').map(_.trim)

  val dictSize = readInt
  val dict = (0 until dictSize) map { _=>
    readLine.trim
  } toSet

  val res = transformWord(to, dict)(from::Nil)

  println(res)

}
