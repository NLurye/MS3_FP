import java.util.concurrent.{Callable, ExecutorService, Future}
import scala.collection.mutable.ListBuffer


case class Report(feature: String, var timeStep: Int, anomalyScore: Double)


trait ParAnomalyDetector {
  type Reports = ListBuffer[Report]

  def map(ts: TimeSeries): Reports

  def reduce(r1: Reports, r2: Reports): Reports

  def detect(ts: TimeSeries, es: ExecutorService, chunks: Int): Vector[Report] = {
    val tss = ts.split(chunks)
    val new_size = ts.length / chunks
    val reportsList: ListBuffer[Reports] = ListBuffer.empty

    for ((t, i) <- tss.zipWithIndex) {
      val callable: Callable[Reports] = new Callable[Reports] {
        override def call(): Reports = map(t)
      }
      val future: Future[Reports] = es.submit(callable)
      val reports: Reports = future.get()

      reports.toList.foreach(r => r.timeStep = r.timeStep + (i * new_size))
      reportsList += reports
    }

    reportsList.reduce(this.reduce).toVector
  }
}
