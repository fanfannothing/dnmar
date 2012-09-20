package dnmar;

import scala.util.Random;

import scalala.scalar._;
import scalala.tensor.::;
import scalala.tensor.mutable._;
import scalala.tensor.dense._;
import scalala.tensor.sparse._;
import scalala.library.Library._;
import scalala.library.Numerics._;
import scalala.library.LinearAlgebra._;
import scalala.library.Statistics._;
import scalala.library.Plotting._;
import scalala.operators.Implicits._;

import scala.collection.mutable.ListBuffer

object Eval {
  case class Prediction(val score:Double, val correct:Boolean) 

  var useObsPredictions = false
  var useAveragedParameters = false

  def HumanEval(param:Parameters, test:EntityPairData, annotatedFile:String) {
    if(Constants.TIMING) {
      Utils.Timer.start("HumanEval")
    }

    /*
     * Read in the features and labels
     */
    val features = new ListBuffer[SparseVectorCol[Double]]
    val labels   = new ListBuffer[Int]
    for(line <- scala.io.Source.fromFile(annotatedFile).getLines()) {
      //var Array(e1id_str, e2id_str, i_dont_know_what_this_is, relation_str, is_mention_str, sentence) = line.trim.split("\t")
      var Array(e1id_str, e2id_str, i_dont_know_what_this_is, relation_str, is_mention_str, sentence_annotated, e1str, e2str, sentence) = line.trim.split("\t")
      if(sentence(0) == '"') {
	sentence = sentence.substring(1,sentence.length-1)	//strip quotes
      }

      if(is_mention_str != "n") {
	test.entityVocab.lock      
	val e1id = test.entityVocab(e1id_str)
	val e2id = test.entityVocab(e2id_str)

	//OK, now let's find the sentence in the test data, so we can get it's features
	val ep    = test.data.filter((ep) => ep.e1id == e1id && ep.e2id == e2id)(0)
	val index = ep.sentences.indexOf(sentence)
	if(index >= 0) {
	  features += ep.xCond(index)
	  labels   += test.relVocab(relation_str)
	} else {
	  if(Constants.DEBUG) {
	    println("Threw out an annotated example...")
	    println(e1id_str + "\t" + e1id)
	    println(e2id_str + "\t" + e2id)
	    println(sentence)
	    println(ep.sentences.toList)
	    println(ep.sentences.indexOf(sentence))
	  }
	}
      }
    }

    var sortedPredictions = List[Prediction]()

    for(i <- 0 until features.length) {
      var postZ:DenseVector[Double] = null
      if(useAveragedParameters) {
	postZ = (param.theta_average * features(i)).toDense
      } else {
	postZ = (param.theta * features(i)).toDense
      }
      val predicted = postZ.argmax
      if(predicted == labels(i)) {
	sortedPredictions ::= Prediction(postZ(predicted), true)
      } else {
	sortedPredictions ::= Prediction(postZ(predicted), false)
      }
    }

    PrintPR(sortedPredictions, features.length)

    if(Constants.TIMING) {
      Utils.Timer.stop("HumanEval")
    }
  }

  def AggregateEval(param:Parameters, test:EntityPairData) = {
    if(Constants.TIMING) {
      Utils.Timer.start("AggregateEval")
    }

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

    PrintPR(sortedPredictions, totalRelations)

    if(Constants.TIMING) {
      Utils.Timer.stop("AggregateEval")
    }
  }

  def PrintPR(sortedPredictions:List[Prediction], maxResults:Double) {
    var tp, fp, fn = 0.0

    var maxF, maxFp, maxFr = 0.0
    var maxP, maxPr, maxPf = 0.0
    for(prediction <- sortedPredictions.sortBy(-_.score)) {
      if(prediction.correct) {
	tp += 1.0
      } else {
	fp += 1.0
      }

      fn = maxResults - tp
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
