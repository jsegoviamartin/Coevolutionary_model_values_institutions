
package coveo
import coveo.Types.Signal

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

}
