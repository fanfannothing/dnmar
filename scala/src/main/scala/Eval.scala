package dnmar;

object Eval {
  case class Prediction(val score:Double, val correct:Boolean) 

  def AggregateEval(param:Parameters, test:EntityPairData) = {
    var tp, fp, fn = 0.0
    var totalRelations = 0.0	//For computing fn

    var sortedPredictions = List[Prediction]()
    for(ep <- test.data) { 
      val predicted = param.inferAll(ep)
      if(Constants.DEBUG) {
	println("predicted:\t" + Utils.bin2int(predicted.rel.toArray).map((r) => test.relVocab(r)))
	println("observed:\t"  + Utils.bin2int(ep.rel.toArray).map((r) => test.relVocab(r)))
      }
      for(r <- 0 until test.nRel) {
	if(test.relVocab(r) != "NA") {
	  if(ep.rel(r) == 1.0) {
	    totalRelations += 1.0
	  }
	  if(ep.rel(r) == 1.0 && predicted.rel(r) == 1.0) { 
	    sortedPredictions ::= Prediction(predicted.zScore(predicted.z :== r).max, true)
	    //tp += 1.0
	  }
	  else if(ep.rel(r) == 0.0 && predicted.rel(r) == 1.0) {
	    sortedPredictions ::= Prediction(predicted.zScore(predicted.z :== r).max, false)
	    //fp += 1.0
	  }
	}
      }
    }

    var maxF, maxFp, maxFr = 0.0
    var maxP, maxPr, maxPf = 0.0
    for(prediction <- sortedPredictions.sortBy(-_.score)) {
      //println(Prediction(-prediction.score, prediction.correct))
      if(prediction.correct) {
	tp += 1.0
      } else {
	fp += 1.0
      }

      fn = totalRelations - tp
      val p = tp / (tp + fp)
      val r = tp / (tp + fn)
      val f = 2 * p * r / (p + r)

      if(f > maxF) {
	maxF  = f
	maxFp = p
	maxFr = r
      }

      if(p > maxP) {
	maxP  = p
	maxPr = r
	maxPf = f
      }
    }
    println("P:" + maxFp + "\tR:" + maxFr + "\tF:" + maxF)
    println("P:" + maxP  + "\tR:" + maxPr + "\tF:" + maxPf)
  }
}
