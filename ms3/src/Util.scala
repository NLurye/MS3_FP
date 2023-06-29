import scala.math.log
import scala.reflect.ClassTag

object Util {
  def probs(xs: Array[Double]): Array[Double] =
    xs.map(xi => xs.count(xj => xi == xj) / xs.size.toDouble)
  def entropy(xs: Array[Double]): Double =
    -(xs zip probs(xs)).distinct.map(pi => pi._2 * log(pi._2) / log(2)).sum

  def remove[A: ClassTag](xs: Array[A], index: Int): Array[A] = {
    val newArr = new Array[A](xs.length - 1)
    var i = 0
    var j = 0
    while (i < xs.length) {
      if (i != index) {
        newArr(j) = xs(i)
        j += 1
      }
      i += 1
    }
    newArr
  }

  def anomaly(xs: Array[Double], index: Int, general_entropy: Double): Double = {
    val removed = remove(xs, index)
    val entropyRemoved = entropy(removed)
    general_entropy - entropyRemoved
  }

  def getMaximumAnomaly(xs: Array[Double]): (Double, Int) = {
    val general_entropy = entropy(xs)
    val anomalies = (Range(0, xs.length)).map(pi => anomaly(xs, pi, general_entropy))
//    println("max: ",anomalies.max, "i",anomalies.indexOf(anomalies.max))
    (anomalies.max, anomalies.indexOf(anomalies.max))
  }

}
