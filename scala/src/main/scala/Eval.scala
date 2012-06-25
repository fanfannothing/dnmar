package dnmar;

object Eval {
  def AggregateEval(param:Parameters, test:EntityPairData) = {
    var tp, fp, fn = 0.0

    for(ep <- test.data) { 
      val predicted = param.inferAll(ep)
      //println("predicted:\t" + predicted.rel.toList)
      //println("observed:\t"  + ep.rel.toList)
      for(r <- 0 until test.nRel) {
	if(test.relVocab(r) != "NA") {
	  if(ep.rel(r) == 1.0 && predicted.rel(r) == 1.0) { 
	    tp += 1.0
	  }
	  else if(ep.rel(r) == 0.0 && predicted.rel(r) == 1.0) { 
	    fp += 1.0
	  }
	  else if(ep.rel(r) == 1.0 && predicted.rel(r) == 0.0) { 
	    fn += 1.0
	  }
	}
      }
    }
    val p = tp / (tp + fp)
    val r = tp / (tp + fn)
    val f = 2 * p * r / (p + r)

    println("P:" + p)
    println("R:" + r)
    println("F:" + f)
  }
}
