package coevo

import scala.util.Random

object Utils {

  implicit class ArrayDecorator(array: Array[Double]) {
    def add(anotherArray: Array[Double]) =
      (array zip anotherArray).map { case (el1, el2) => el1 + el2 }

    def mean = {
      array.sum / array.size
    }
  }


  def multinomial[T](values: Array[(T, Double)], random: Random): T = {
    var sum = 0.0
    var previousSum = 0.0
    val cdf = Array.ofDim[Double](values.length + 1)

    for {
      i <- values.indices
    } {
      sum = previousSum + values(i)._2
      cdf(i + 1) = sum
      previousSum = sum
    }

    import collection.Searching._

    val drawn = random.nextDouble() * sum
    val drawnIndex = cdf.search(drawn)

    values(drawnIndex.insertionPoint - 1)._1
  }

  def uniform[T](values: Array[T], size: Int, random: Random) = {
    val maxBound = values.size - 1
    (1 to size).map { i =>
      values(random.between(0, maxBound))
    }
  }

  def entropy(seq: Seq[Int]) = {
    val n = seq.sum
    val probs = seq.filter {
      _ > 0
    }.map {
      _.toDouble / n
    }
    -probs.map { x => x * Math.log(x) / Math.log(2) }.sum
  }

}
