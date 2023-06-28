import scala.collection.mutable.ListBuffer

object EntropyAnomalyDetector extends ParAnomalyDetector {

  override def map(ts: TimeSeries): Reports = {
    ListBuffer.empty ++
      ts.data
        .map(e => (e._1, Util.getMaximumAnomaly(e._2.toArray)))
        .map(e => Report(e._1, e._2._2, e._2._1))
  }

  override def reduce(r1: Reports, r2: Reports): Reports = {
    ListBuffer.empty ++ (r1 ++ r2).groupBy(_.feature).map(_._2.maxBy(_.anomalyScore)).toArray.sortBy(_.feature)
  }
}