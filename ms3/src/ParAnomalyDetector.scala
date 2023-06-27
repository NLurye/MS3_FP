import java.util.concurrent.{ExecutorService, Future}
import scala.collection.mutable.ListBuffer


case class Report(feature: String, var timeStep: Int, anomalyScore: Double)


trait ParAnomalyDetector {
  type Reports = ListBuffer[Report]

  def map(ts: TimeSeries): Reports

  def reduce(r1: Reports, r2: Reports): Reports

  def detect(ts: TimeSeries, es: ExecutorService, chunks: Int): Vector[Report] = {
    val splitted_ts: List[TimeSeries] = ts.split(chunks)
    val reports_list: ListBuffer[Future[Reports]] = ListBuffer.empty[Future[Reports]]
    for (i <- 0 until chunks) {
      reports_list += es.submit(() => map(splitted_ts(i)))
    }
    val allReports: Reports = ListBuffer.empty[Report]
    for (i <- 0 until chunks) {
      val report = reports_list(i).get()
      allReports ++= report

      if (i != chunks - 1) {
        val nextReport = reports_list(i + 1).get()
        allReports ++= reduce(report, nextReport)
      }

    }

    allReports.toVector
  }
}
//
//def par[A](as: IndexedSeq[A])(bf: (A, A) => A): A =
//  if (as.size == 1)
//    as.head
//  else {
//    println(as)
//    val (l, r) = as.splitAt(as.length / 2)
//    val fl = es.submit(() => par(l)(bf)) // Java Future
//    val fr = es.submit(() => par(r)(bf)) // Java Future
//    bf(fl.get(), fr.get())
//  }