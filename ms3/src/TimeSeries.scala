import scala.collection.mutable
import scala.io.Source


class TimeSeries(csvFileName:String) {
  val data=new mutable.HashMap[String,Vector[Double]]()
  private val dataBuilder=new mutable.HashMap[String,mutable.ArrayBuffer[Double]]()
  private val source=Source.fromFile(csvFileName)
  //  val length = source.getLines().size
  val features=source.getLines().next().split(",").toVector
  features.foreach(f=>dataBuilder.put(f,mutable.ArrayBuffer[Double]()))
  source.getLines().foreach(line=>{
        val row=line.split(",")
        for(i<-0 until row.length) {
          if(dataBuilder.contains(features(i)))
            dataBuilder.get(features(i)).get += row(i).toDouble
        }
  })
  source.close()
  dataBuilder.foreach(t=>data.put(t._1,t._2.toVector))

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

  def this(data: Map[String, Vector[Double]]) {
    this("")
    this.data ++= data
  }
  
}
