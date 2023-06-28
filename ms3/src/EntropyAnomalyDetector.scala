import scala.collection.mutable.ListBuffer

object EntropyAnomalyDetector extends ParAnomalyDetector {

  override def map(ts: TimeSeries): Reports = {
    val reports: ListBuffer[Report] = ListBuffer.empty
    for (e <- ts.data) {
      val maximumAnomaly = Util.getMaximumAnomaly(e._2.toArray)
      val report = Report(e._1, maximumAnomaly._2, maximumAnomaly._1)
      reports += report
    }
    reports
  }

  override def reduce(r1: Reports, r2: Reports): Reports = {
    ListBuffer.empty ++ (r1 ++ r2).groupBy(_.feature).map(_._2.maxBy(_.anomalyScore)).toArray.sortBy(_.feature)
  }
}