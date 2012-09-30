package dnmar;

import java.util.zip.GZIPInputStream
import java.io.FileInputStream
import java.io.BufferedInputStream
import java.io.InputStream

import cc.factorie.protobuf.DocumentProtos.Relation
import cc.factorie.protobuf.DocumentProtos.Relation.RelationMentionRef

import org.clapper.argot._

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

  def main(args: Array[String]) {
    try { 
      parser.parse(args)
    }
    catch { 
      case e: ArgotUsageException => println(e.message)
    }

    val multir = new MultiR(train.value.getOrElse(null))
    val dnmar  = new DNMAR(train.value.getOrElse(null))

    println("evaluating MultiR")
    EvalIterations(multir)

    //println("evaluating DNMAR")
    //EvalIterations(dnmar)
  }

  def EvalIterations(dnmar:Parameters) {
    val nrel     = train.value.getOrElse(null).relVocab.size
    val relVocab = train.value.getOrElse(null).relVocab

    for(i <- 0 to 150) {
      println("*********************************************")
      println("iteration " + i)
      println("*********************************************")

      dnmar.train(1)

      println("rel predictions:")
      Eval.useObsPredictions = false
      Eval.AggregateEval(dnmar, test.value.getOrElse(null))
      if(i % 10 && i >= 10) {
	for(r <- 0 until nrel) {
	  println(relVocab(r))
	  Eval.AggregateEval(dnmar, test.value.getOrElse(null), r)
	}
      }

      println("*********************************************")
      println("* rel predictions (training):")
      println("*********************************************")
      Eval.useObsPredictions = false
      Eval.AggregateEval(dnmar, train.value.getOrElse(null))

      println("*********************************************")
      println("* Human annotated evaluation")
      println("*********************************************")
      Eval.HumanEval(dnmar, test.value.getOrElse(null), "/home/aritter/dlvm/multir-release/annotations/sentential.txt")
      if(i % 10 && i >= 10) {
	for(r <- 0 until nrel) {
	  println(relVocab(r))
	  Eval.HumanEval(dnmar, test.value.getOrElse(null), "/home/aritter/dlvm/multir-release/annotations/sentential.txt", r)
	}
      }

      if(i % 10 == 0 && i >= 10) {
	println("*********************************************")
	println("* averaged parameters")
	println("*********************************************")
	dnmar.computeThetaAverage
	Eval.useAveragedParameters = true

	Eval.useObsPredictions = false
	Eval.AggregateEval(dnmar, test.value.getOrElse(null))
	for(r <- 0 until nrel) {
	  println(relVocab(r))
	  Eval.AggregateEval(dnmar, test.value.getOrElse(null), r)
	}

	println("Human annotated evaluation (averaged)")
	Eval.HumanEval(dnmar, test.value.getOrElse(null), "/home/aritter/dlvm/multir-release/annotations/sentential.txt")
	for(r <- 0 until nrel) {
	  println(relVocab(r))
	  Eval.HumanEval(dnmar, test.value.getOrElse(null), "/home/aritter/dlvm/multir-release/annotations/sentential.txt", r)
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
