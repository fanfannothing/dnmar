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

import java.io._

object Eval {
  //case class Prediction(val score:Double, val correct:Boolean) 
  class Prediction(val score:Double, val correct:Boolean, val rel:String, val annotated_sentence:String) {
    def this(score:Double, correct:Boolean) = this(score, correct, null, null)
  }

  var useObsPredictions = false
  var useAveragedParameters = false

  def HumanEval(param:Parameters, test:EntityPairData, annotatedFile:String, rel:Int) {
    HumanEval(param, test, annotatedFile, rel, null)
  }

  def HumanEval(param:Parameters, test:EntityPairData, annotatedFile:String) {
    HumanEval(param, test, annotatedFile, -1, null)
  }

  def HumanEval(param:Parameters, test:EntityPairData, annotatedFile:String, outFile:String) {
    HumanEval(param, test, annotatedFile, -1, outFile)
  }

  def HumanEval(param:Parameters, test:EntityPairData, annotatedFile:String, rel:Int, outFile:String) {
    if(Constants.TIMING) {
      Utils.Timer.start("HumanEval")
    }

    /*
     ************************************************************************************************************************
     * NOTE: it appears MultiR uses the aggregate-level scores to generate P/R curves rather than the per-sentence scores
     * Hypothesis: this could lead to higher recal....
     *
     * Comment From MultiR (FullInference.java):
       It's important to ignore the _NO_RELATION_ type here, soneed to start at 1!
       final value is avg of maxes
     ************************************************************************************************************************
     */

    /*
     * Read in the features and labels
     */
    val features            = new ListBuffer[SparseVectorCol[Double]]
    val aggregateScores     = new ListBuffer[Double]
    val labels              = new ListBuffer[Int]
    val sentences           = new ListBuffer[String]
    val sentences_annotated = new ListBuffer[String]
    for(line <- scala.io.Source.fromFile(annotatedFile).getLines()) {
      //var Array(e1id_str, e2id_str, i_dont_know_what_this_is, relation_str, is_mention_str, sentence) = line.trim.split("\t")
      var Array(e1id_str, e2id_str, i_dont_know_what_this_is, relation_str, is_mention_str, sentence_annotated, e1str, e2str, sentence) = line.trim.split("\t")
      if(sentence(0) == '"') {
	sentence = sentence.substring(1,sentence.length-1)	//strip quotes
      }

      //if(is_mention_str != "n" && (rel == -1 || test.relVocab(relation_str) == rel)) {
      //if(rel == -1 || test.relVocab(relation_str) == rel && (relation_str != "/location/administrative_division/country") && (is_mention_str != "")) {
      //if(rel == -1 || test.relVocab(relation_str) == rel && (relation_str != "/location/administrative_division/country")) {
      if(relation_str != "/location/administrative_division/country") {
	test.entityVocab.lock      
	val e1id = test.entityVocab(e1id_str)
	val e2id = test.entityVocab(e2id_str)

	//Treat errors as "NA"? (doesn't penalize recall for missing them, but any predictions will hurt precision...)
	if(is_mention_str != "y" && is_mention_str != "indirect") {
	  relation_str = "NA"
	}

	val ep    = test.data.filter((ep) => ep.e1id == e1id && ep.e2id == e2id)(0)
	//TODO: no need to find the actual test sentence if we'er using the aggregate scores?
	//OK, now let's find the sentence in the test data, so we can get it's features
	val index = ep.sentences.indexOf(sentence)
	if(index >= 0) {
	  features            += ep.features(index)
	  labels              += test.relVocab(relation_str)
	  sentences           += sentence
	  sentences_annotated += sentence_annotated
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

    var maxRecall = 0.0
    for(i <- 0 until features.length) {
      var postZ:DenseVector[Double] = null
      if(useAveragedParameters) {
	postZ = (param.theta_average * features(i)).toDense
      } else {
	postZ = (param.theta * features(i)).toDense
      }
      val logExpSum = MathUtils.LogExpSum(postZ.toArray)
      postZ -= logExpSum
      val pNA = postZ(test.relVocab("NA"))
      postZ(test.relVocab("NA")) = Double.NegativeInfinity
      var predicted = postZ.argmax

      //println(param.data.relVocab(predicted) + "\t" + sentences(i))

      if(labels(i) != test.relVocab("NA") && (rel == -1 || labels(i) == rel)) {
	maxRecall += 1.0
      }
      
      //if(predicted != test.relVocab("NA")) {
      if((rel == -1 || predicted == rel) && predicted != test.relVocab("NA")) {
	if(predicted == labels(i)) {
	  //True Positive
	  sortedPredictions ::= new Prediction(postZ(predicted), true, test.relVocab(predicted), sentences_annotated(i))
	//} else if(labels(i) != test.relVocab("NA")) {
	} else {
	  //False Positive
	  sortedPredictions ::= new Prediction(postZ(predicted), false, test.relVocab(predicted), sentences_annotated(i))
	}
      }
    }

    if(maxRecall > 0) {
      if(outFile != null) {
	println("dumping to " + outFile)
	DumpPR(sortedPredictions, maxRecall, outFile)
      }
      PrintPR(sortedPredictions, maxRecall)
    }

    if(Constants.TIMING) {
      Utils.Timer.stop("HumanEval")
    }
  }

  def AggregateEval(param:Parameters, test:EntityPairData) {
    AggregateEval(param, test, -1, null)
  }

  def AggregateEval(param:Parameters, test:EntityPairData, rel:Int) {
    AggregateEval(param, test, rel, null)
  }

  def AggregateEval(param:Parameters, test:EntityPairData, outFile:String) {
    AggregateEval(param, test, -1, outFile)
  }

  def AggregateEval(param:Parameters, test:EntityPairData, rel:Int, outFile:String) {
    if(Constants.TIMING) {
      Utils.Timer.start("AggregateEval")
    }

    var totalRelations = 0.0	//For computing fn

    var sortedPredictions = List[Prediction]()
    for(ep <- Random.shuffle(test.data.toList)) { 
      val predicted = param.inferAll(ep, useAveragedParameters)
//      if(Constants.DEBUG) {
//	println("predicted:\t" + Utils.bin2int(predicted.rel.toArray).map((r) => test.relVocab(r)))
//	println("observed:\t"  + Utils.bin2int(ep.rel.toArray).map((r) => test.relVocab(r)))
//      }
      for(r <- 0 until test.nRel) {
	if(test.relVocab(r) != "NA" && (rel == -1 || r == rel)) {
	  if(ep.rel(r) == 1.0 && (rel == -1 || rel == r)) {
	    totalRelations += 1.0
	  }

	  val prediction = if(useObsPredictions) { 
	    predicted.obs(r)
	  } else {
	    predicted.rel(r)
	  }

	  if(ep.rel(r) == 1.0 && prediction == 1.0) { 
	    sortedPredictions ::= new Prediction(predicted.zScore(predicted.z :== r).max, true)
	    //sortedPredictions ::= Prediction(predicted.zScore(predicted.z :== r).max, true)
	  }
	  else if(ep.rel(r) == 0.0 && prediction == 1.0) {
	    sortedPredictions ::= new Prediction(predicted.zScore(predicted.z :== r).max, false)
	    //sortedPredictions ::= Prediction(predicted.zScore(predicted.z :== r).max, false)
	  }
	}
      }
    }
    
    if(outFile != null) {
      DumpPR(sortedPredictions, totalRelations, outFile)
    }
    
    PrintPR(sortedPredictions, totalRelations)

    if(Constants.TIMING) {
      Utils.Timer.stop("AggregateEval")
    }
  }

  def DumpPR(sortedPredictions:List[Prediction], maxResults:Double, outFile:String) {
    var tp, fp, fn = 0.0

    val fw = new FileWriter(outFile)

    for(prediction <- sortedPredictions.sortBy(-_.score)) {
      if(prediction.correct) {
	tp += 1.0
      } else {
	fp += 1.0
      }

      fn = maxResults - tp
      val p = tp / (tp + fp)
      val r = tp / (tp + fn)
      //val f = 2 * p * r / (p + r)

      fw.write(p + "\t" + r + "\t" + prediction.rel + "\t" + prediction.correct + "\t" + prediction.annotated_sentence + "\n")
    }

    fw.close()
  }    

  def PrintPR(sortedPredictions:List[Prediction], maxResults:Double) {
    var tp, fp, fn = 0.0

    var maxF,  maxFp, maxFr = 0.0
    var maxP,  maxPr, maxPf = 0.0
    var maxRp, maxR,  maxRf = 0.0
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

      if(r > maxR && p > 0.5) {
	maxR  = r
	maxRp = p
	maxRf = f
      }
    }
    println("N:" + sortedPredictions.length)
    println("P:" + maxFp + "\tR:" + maxFr + "\tF:" + maxF)
    println("P:" + maxP  + "\tR:" + maxPr + "\tF:" + maxPf)
    println("P:" + maxRp + "\tR:" + maxR  + "\tF:" + maxRf)
  }
}
