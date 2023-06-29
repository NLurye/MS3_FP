import java.util.concurrent.{Callable, ExecutorService, Future}
import scala.collection.mutable.ListBuffer


//case class Report(feature: String, var timeStep: Int, anomalyScore: Double)
case class Report(feature: String, var timeStep: Int, anomalyScore: Double, value: Double)


trait ParAnomalyDetector {
  type Reports = ListBuffer[Report]

  def map(ts: TimeSeries): Reports

  def reduce(r1: Reports, r2: Reports): Reports

  def detect(ts: TimeSeries, es: ExecutorService, chunks: Int): Vector[Report] = {
    val splitted_ts = ts.split(chunks)
    val new_size = ts.length / chunks
    val reports_list: ListBuffer[Reports] = ListBuffer.empty

    for ((t, i) <- splitted_ts.zipWithIndex) {
      val callable: Callable[Reports] = new Callable[Reports] {
        override def call(): Reports = map(t)
      }
      val future: Future[Reports] = es.submit(callable)
      val all_reports: Reports = future.get()
      for (r <- all_reports) {
      println("[f: ", r.feature,"time: ", r.timeStep, " anomalyScore: ", r.anomalyScore, "VALUE: ", r.value)
      }


        all_reports.toList.foreach(r => r.timeStep = r.timeStep + (i * new_size))
      reports_list += all_reports
    }

    reports_list.reduce(this.reduce).toVector
  }
}
