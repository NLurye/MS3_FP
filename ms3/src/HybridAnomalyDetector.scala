object HybridAnomalyDetector extends AnomalyDetector {
  override def learn(normal: TimeSeries): Map[String, String] = {
    val model = new mutable.HashMap[String, String]()
    val cf = Util.correlationFinder(normal)
    cf.foreach(p => {
      if (p._3 >= 0.9) {
        val xs = normal.getValues(normal.features(p._1)).get.toArray
        val ys = normal.getValues(normal.features(p._2)).get.toArray
        val regLine = new Line(Util.toPoints(xs, ys))
        val maxDist = Util.toPoints(xs, ys).map(p => regLine.dist(p)).max
        model.put(normal.features(p._1) + "," + normal.features(p._2), "L," + regLine.a + "," + regLine.b + "," + maxDist)
      }
      else if (p._3 > 0.5) {
        val xs = normal.getValues(normal.features(p._1)).get.toArray
        val ys = normal.getValues(normal.features(p._2)).get.toArray
        val ps = Util.toPoints(xs, ys)
        val center = ps.minBy(p => Util.sumSqrDist(p, ps))
        val maxDist = ps.map(p => Util.dist(p, center)).max
        model.put(normal.features(p._1) + "," + normal.features(p._2), "S," + maxDist)
      }
      else {
        val xs = normal.getValues(normal.features(p._1)).get.toArray
        model.put(normal.features(p._1), "Z," + xs.map(x => Math.abs(Util.zscore(xs, x))).max)
      }

    })
    model.toMap
  }

  override def detect(model: Map[String, String], test: TimeSeries): Vector[(String, Int)] = {
    var l = new mutable.ArrayBuffer[(String, Int)]
    model.foreach(p => {
      val f1 = p._1.split(",")(0)
      val f2 = if (p._1.split(",").length == 2) p._1.split(",")(1) else ""
      if (p._2.startsWith("L")) {
        val a = p._2.split(",")(1).toDouble
        val b = p._2.split(",")(2).toDouble
        val maxDist = p._2.split(",")(3).toDouble
        val ps = Util.toPoints(test.getValues(f1).get.toArray, test.getValues(f2).get.toArray)
        var i = 0
        ps.foreach(p => {
          if (Math.abs(a * p.x + b - p.y) > maxDist) { // then there is an anomaly
            l += Tuple2(f1 + "," + f2, i)
          }
          i = i + 1
        })

      } else if (p._2.startsWith("S")) {
        val xs = test.getValues(f1).get.toArray
        val ys = test.getValues(f2).get.toArray
        val ps = Util.toPoints(xs, ys)
        val center = ps.minBy(p => Util.sumSqrDist(p, ps))
        val maxDist = p._2.split(",")(1).toDouble
        var i = 0
        ps.foreach(p => {
          if (Util.dist(center, p) > maxDist) { // then there is an anomaly
            l += Tuple2(f1 + "," + f2, i)
          }
          i = i + 1
        })

      } else if (p._2.startsWith("Z")) {
        val xs = test.getValues(f1).get.toArray
        val maxZ = p._2.split(",")(1).toDouble
        var i = 0
        xs.map(x => Math.abs(Util.zscore(xs, x))).foreach(z => {
          if (z > maxZ)
            l += Tuple2(f1, i)
          i += 1
        })

      }
    })
    l.toVector
  }
}
