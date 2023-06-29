import scala.collection.mutable.ListBuffer

object EntropyAnomalyDetector extends ParAnomalyDetector {

  override def map(ts: TimeSeries): Reports = {
    val reports: ListBuffer[Report] = ListBuffer.empty
    for (e <- ts.data) {
      val maximumAnomaly = Util.getMaximumAnomaly(e._2.toArray)
//      val report = Report(e._1, maximumAnomaly._2, maximumAnomaly._1)
        val report = Report(e._1, maximumAnomaly._2, maximumAnomaly._1, e._2(maximumAnomaly._2))
      reports += report
    }
    reports
  }

  override def reduce(r1: Reports, r2: Reports): Reports = {
    val mergedReports = r1 ++ r2
    val groupedReports = mergedReports.groupBy(_.feature)
    val selectedReports = ListBuffer.empty[Report]
    for ((_, reports) <- groupedReports) {
      var maxScoreReport = reports.head
      for (report <- reports.tail) {
        if (report.anomalyScore > maxScoreReport.anomalyScore) {
          maxScoreReport = report
        }
      }
      selectedReports += maxScoreReport
    }
    val sortedReports = selectedReports.toSeq.sortBy(_.feature)
    val result = ListBuffer.empty[Report]
    result ++= sortedReports
    result
  }
}