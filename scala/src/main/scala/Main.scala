package dnmar;

import java.util.zip.GZIPInputStream
import java.io.FileInputStream
import java.io.BufferedInputStream
import java.io.InputStream

import cc.factorie.protobuf.DocumentProtos.Relation
import cc.factorie.protobuf.DocumentProtos.Relation.RelationMentionRef

import sys.process._

import org.clapper.argot._

import java.io._

object Constants {
  var DEBUG = false
  var TIMING = true
}

object Main {
  import ArgotConverters._
  
  val parser = new ArgotParser(
    "DNMAR",
    preUsage=Some("DNMAR: Version 0.1. Copyright (c) " +
                  "2012, Alan Ritter.")
  )

  val train = parser.option[ProtobufData](List("trainProto"), "n", "Training data (in google protobuf format).") { 
    println("Loading train")
    (sValue, opt) => new ProtobufData(sValue)
  }

  val test  = parser.option[ProtobufData](List("testProto"), "n", "Test data (in google protobuf format).") {
    println("Loading test")
    (sValue, opt) => new ProtobufData(sValue, 
				      train.value.getOrElse(null).entityVocab, 
				      train.value.getOrElse(null).relVocab.lock, 
				      train.value.getOrElse(null).featureVocab.lock,
				      true)
  }

  val outDir = parser.option[String](List("outDir"), "n", "output directory") {
    (sValue, opt) => sValue
  }

  val outCompareInfer = parser.option[String](List("outCompareInfer"), "n", "output file for comparing inference methods") {
    (sValue, opt) => sValue
  }

  def main(args: Array[String]) {
    try { 
      parser.parse(args)
    }
    catch { 
      case e: ArgotUsageException => println(e.message)
    }

    val multir = new MultiR(train.value.getOrElse(null))
    val dnmar  = new DNMAR(train.value.getOrElse(null))

    if(outDir.value.getOrElse(null) != null) {
      println(outDir.value.getOrElse(null))
      ("mkdir -p " + outDir.value.getOrElse(null)).!
    }

    if(false) {
    //if(true) {
      println("evaluating MultiR")
      EvalIterations(multir, 50)
    } else {
      println("evaluating DNMAR")
      EvalIterations(dnmar, 1)
    }
  }

  def EvalIterations(dnmar:Parameters, nIter:Int) {
    val nrel     = train.value.getOrElse(null).relVocab.size
    val relVocab = train.value.getOrElse(null).relVocab

    var fw:FileWriter = null
    if(outCompareInfer.value.getOrElse(null) != null) {
      println(outCompareInfer.value.getOrElse(null))
      fw = new FileWriter(outCompareInfer.value.getOrElse(null))
      fw.write(List("score1rs", "time1rs", "score10rs", "time10rs", "score20rs", "time20rs", "score1kBeam", "time1kBeam", "scoreBNB", "timeBNB", "scoreExact", "timeExact", "nVars").reduceLeft(_ + "\t" + _) + "\n")
    }

    //for(i <- 0 to nIter+1) {
    for(i <- 0 to nIter) {
      var outFile:String = null

      println("*********************************************")
      println("iteration " + i)
      println("*********************************************")

      dnmar.train(1, fw)

      /*
      println("rel predictions:")
      Eval.useObsPredictions = false
      Eval.AggregateEval(dnmar, test.value.getOrElse(null))

      println("*********************************************")
      println("* rel predictions (training):")
      println("*********************************************")
      Eval.useObsPredictions = false
      Eval.AggregateEval(dnmar, train.value.getOrElse(null))

      println("*********************************************")
      println("* Human annotated evaluation")
      println("*********************************************")
      Eval.HumanEval(dnmar, test.value.getOrElse(null), "/home/aritter/dlvm/multir-release/annotations/sentential.txt")
      */

      //if(i % 10 == 0 && i >= 10 || i == nIter) {
      if(i == nIter) {
	println("*********************************************")
	println("* averaged parameters")
	println("*********************************************")
	dnmar.computeThetaAverage
	Eval.useAveragedParameters = true

	Eval.useObsPredictions = false

	if(i == nIter) {
	  outFile = outDir.value.getOrElse(null)
	  if(outFile != null) {
	    outFile += "/aggregate"
	  }
	}
	Eval.AggregateEval(dnmar, test.value.getOrElse(null), outFile)

	println("Human annotated evaluation (averaged)")
	if(i == nIter) {
	  outFile = outDir.value.getOrElse(null)
	  if(outFile != null) {
	    outFile += "/sentential"
	  }
	}
	Eval.HumanEval(dnmar, test.value.getOrElse(null), "/home/aritter/dlvm/multir-release/annotations/sentential.txt", outFile)
	for(r <- 0 until nrel) {
	  if(i == nIter) {
	    outFile = outDir.value.getOrElse(null)
	    if(outFile != null) {
	      outFile += ("/sentential_" + relVocab(r).replace("/", "_"))
	    }
	  }
	  println(relVocab(r))
	  Eval.HumanEval(dnmar, test.value.getOrElse(null), "/home/aritter/dlvm/multir-release/annotations/sentential-byrelation.txt", r, outFile)
	}
	Eval.useAveragedParameters = false
      }

      if(Constants.TIMING) {
	Utils.Timer.print
	Utils.Timer.reset
      }
    }
  }
  
  if(Constants.TIMING) {
    Utils.Timer.print
  }
}
