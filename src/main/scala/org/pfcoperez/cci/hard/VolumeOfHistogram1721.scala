package org.pfcoperez.cci.hard

object VolumeOfHistogram1721 extends App {

  def volumeOfHistogram(histogram: Vector[Int]): Int = if(histogram.length == 0) 0 else {
    val fromRightHighestSofar = (histogram :\ Vector.empty[Int]) {
      case (v,Vector()) => Vector(v)
      case (v,sofar) => math.max(v, sofar.head) +: sofar
    }
    val fromLeftHighestSofar = (Vector.empty[Int] /: histogram) {
      case (Vector(), v) => Vector(v)
      case (sofar, v) => sofar :+ math.max(v, sofar.last)
    }
    val limits = histogram zip (fromLeftHighestSofar zip fromRightHighestSofar)
    val (sum, _) = ((0, limits.head._2) /: limits.tail) {
      case ((sum, prevLimits @ (prevlh, prevrh)), (v, limits @ (lh, rh))) =>
        if(prevLimits != limits && prevlh < prevrh) {
          (sum, limits)
        } else
          (sum+math.min(lh,rh)-v, limits)
    }
    sum
  }

  val input = readLine.split(' ').map(_.trim.toInt)

  println(volumeOfHistogram(input.toVector))

  /** Samples:
    *
    * 0 0 4 0 0 6 0 0 3 0 8 0 2 0 5 2 0 3 0 0 -> 46
    * 0 0 4 0 0 6 0 0 3 0 5 0 1 0 0 0 -> 26
    *
    */

}
