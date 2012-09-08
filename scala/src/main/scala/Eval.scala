package dnmar;

import scala.util.Random;

object Eval {
  case class Prediction(val score:Double, val correct:Boolean) 

  //var useObsPredictions = true
  var useObsPredictions = false
  var useAveragedParameters = false

  def AggregateEval(param:Parameters, test:EntityPairData) = {
    var tp, fp, fn = 0.0
    var totalRelations = 0.0	//For computing fn

    var sortedPredictions = List[Prediction]()
    for(ep <- Random.shuffle(test.data.toList)) { 
      val predicted = param.inferAll(ep, useAveragedParameters)
      if(Constants.DEBUG) {
	println("predicted:\t" + Utils.bin2int(predicted.rel.toArray).map((r) => test.relVocab(r)))
	println("observed:\t"  + Utils.bin2int(ep.rel.toArray).map((r) => test.relVocab(r)))
      }
      for(r <- 0 until test.nRel) {
	if(test.relVocab(r) != "NA") {
	  if(ep.rel(r) == 1.0) {
	    totalRelations += 1.0
	  }

	  val prediction = if(useObsPredictions) { 
	    predicted.obs(r)
	  } else {
	    predicted.rel(r)
	  }

	  if(ep.rel(r) == 1.0 && prediction == 1.0) { 
	    sortedPredictions ::= Prediction(predicted.zScore(predicted.z :== r).max, true)
	  }
	  else if(ep.rel(r) == 0.0 && prediction == 1.0) {
	    sortedPredictions ::= Prediction(predicted.zScore(predicted.z :== r).max, false)
	  }
	}
      }
    }

    var maxF, maxFp, maxFr = 0.0
    var maxP, maxPr, maxPf = 0.0
    for(prediction <- sortedPredictions.sortBy(-_.score)) {
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

      if(r > 0.05 && p > maxP) {
	maxP  = p
	maxPr = r
	maxPf = f
      }
    }
    println("P:" + maxFp + "\tR:" + maxFr + "\tF:" + maxF)
    println("P:" + maxP  + "\tR:" + maxPr + "\tF:" + maxPf)
  }
}
