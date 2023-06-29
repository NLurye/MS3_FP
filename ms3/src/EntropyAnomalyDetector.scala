import scala.collection.mutable.ListBuffer

object EntropyAnomalyDetector extends ParAnomalyDetector {

  override def map(ts: TimeSeries): Reports = {
    val reports: ListBuffer[Report] = ListBuffer.empty
    for (e <- ts.data) {
      val maximumAnomaly = Util.getMaximumAnomaly(e._2.toArray)
      val report = Report(e._1, maximumAnomaly._2, maximumAnomaly._1, e._2.toArray)
//        val report = Report(e._1, maximumAnomaly._2, maximumAnomaly._1, e._2(maximumAnomaly._2))
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
        // Edge case- if there is a single non-anomalous feature in one of the chunks, we need
        // to be careful not to choose it over a real anomaly in another chunk
        // also, if the entropies in each chunk are similar, then there may not be an anomaly

        if (report.anomalyScore > maxScoreReport.anomalyScore * 1.5) {
          maxScoreReport = report
        }
        else {
          // check that the suspected anomaly is far away from the mean
          val xs_mean = report.xs.sum / report.xs.length
          if (math.abs(report.xs(report.timeStep % report.xs.length) - xs_mean) >  math.abs(maxScoreReport.xs(maxScoreReport.timeStep % maxScoreReport.xs.length) - xs_mean)) {
            maxScoreReport = report
          }
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