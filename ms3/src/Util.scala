import scala.math.log
import scala.reflect.ClassTag

object Util {

  def max[A](as: List[A], comp: (A, A) => Int): A = {
    var mx = as.head
    if (as.size == 1)
      return mx
    val a = max(as.tail, comp)
    if (comp(mx, a) < 0)
      mx = a
    mx
  }

  def map[A, B, C](as: List[A], fab: A => B, fbc: B => C): List[C] =
    as.map(fab).map(fbc)

  def isSorted[A](as: List[A], ordered: (A, A) => Boolean): Boolean = {
    if (as.size == 1)
      return true
    if (ordered(as.head, as.tail.head))
      isSorted(as.tail, ordered)
    else
      false
  }

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
    var maxAnomaly = Double.MinValue
    var maxIndex = 0
    var i = 0
    val general_entropy = entropy(xs)
//    println("general: ",general_entropy)
    while (i < xs.length) {
      val currentAnomaly = anomaly(xs, i, general_entropy)
      if (currentAnomaly > maxAnomaly) {
//        println("updated from: ", maxAnomaly, "to: ", currentAnomaly)
        maxAnomaly = currentAnomaly
        maxIndex = i
      }
      i += 1
    }
    (maxAnomaly, maxIndex)
  }

}
