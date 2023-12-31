import scala.collection.mutable
import scala.io.Source


class TimeSeries(csvFileName:String) {
  val data=new mutable.HashMap[String,Vector[Double]]()
  var length: Int = 0
  var features:Vector[String] = Vector.empty[String]
  private val source = if (csvFileName.nonEmpty) Some(Source.fromFile(csvFileName))
    else None
  if (source.isDefined) {
    val dataBuilder=new mutable.HashMap[String,mutable.ArrayBuffer[Double]]()
    features=source.get.getLines().next().split(",").toVector
  features.foreach(f=>dataBuilder.put(f,mutable.ArrayBuffer[Double]()))
  source.get.getLines().foreach(line=>{
        val row=line.split(",")
        for(i<-0 until row.length) {
          if(dataBuilder.contains(features(i)))
            dataBuilder.get(features(i)).get += row(i).toDouble
        }
  })
  source.get.close()
  dataBuilder.foreach(t=>data.put(t._1,t._2.toVector))
    length = data.values.head.size
}

  // given name of a feature return in O(1) its value series
  def getValues(feature:String):Option[Vector[Double]]=
    if(data.contains(feature))
      Some(data.get(feature).get)
    else
      None

  // given name of a feature return in O(1) its value at the given time step
  def getValue(feature:String,timeStep:Int):Option[Double]=
    if(data.contains(feature) && timeStep>=0 && timeStep<data.get(feature).get.length) {
      Some(data.get(feature).get(timeStep))
    } else
      None

  // given name of a feature return its value series in the range of indices
  def getValues(feature:String,r:Range):Option[Vector[Double]]=
    if(data.contains(feature) && r.start>=0 && r.end<data.get(feature).get.length)
      Some(r.map(i=>data.get(feature).get(i)).toVector)
    else
      None

  def this(data: Map[String, Vector[Double]], old_length: Int, old_features: Vector[String]) {
    this("")
    this.data ++= data
    this.length = old_length
    this.features = old_features
  }

  def split(n: Int): List[TimeSeries] = {
    if (length == 0) {
      return List()
    }
    if (n==1 || length < 2 * n) {
      return List(this)
    }
    val numRows = length
    val chunkSize = numRows / n
    var remainder = numRows % n

    var endIndex = numRows
    val splitSeries = for (_ <- 1 to n) yield {
      val extraChunk = if (remainder > 0) 1 else 0
      val startIndex = endIndex - (chunkSize + extraChunk)
      val chunkRange = startIndex until endIndex
      val chunkData = data.map { case (feature, values) =>
        feature -> values.slice(chunkRange.start, chunkRange.end)
      }
      endIndex = startIndex
      remainder -= extraChunk
      new TimeSeries(chunkData.toMap, chunkRange.length, features)
    }
    splitSeries.toList.reverse
  }

}
