object ZAnomalyDetector extends AnomalyDetector {
  override def learn(normal: TimeSeries): Map[String, String] = {
    val model = new mutable.HashMap[String, String]()
    normal.features.foreach(f => {
      val xs = normal.getValues(f).get.toArray
      model.put(f, "" + xs.map(x => Math.abs(Util.zscore(xs, x))).max)
    })
    model.toMap
  }

  override def detect(model: Map[String, String], test: TimeSeries): Vector[(String, Int)] = {
    var l = new mutable.ArrayBuffer[(String, Int)]
    test.features.foreach(f => {
      val xs = test.getValues(f).get.toArray
      var i = 0
      xs.map(x => Math.abs(Util.zscore(xs, x))).foreach(z => {
        if (z > model.get(f).get.toDouble)
          l += Tuple2(f, i)
        i += 1
      })
    })
    l.toVector
  }
}
