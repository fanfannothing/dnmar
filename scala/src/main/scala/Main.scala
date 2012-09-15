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

    //val dnmar = new DNMAR_greedy(train.value.getOrElse(null))

    /*
     * MultiR
     */
    if(false) {
      println("evaulating MultiR")
      val multir = new MultiR(train.value.getOrElse(null))
      for(i <- 0 to 100) {
	println("iteration " + i)

	multir.train(1)

	println("rel predictions:")
	Eval.useObsPredictions = false
	Eval.AggregateEval(multir, test.value.getOrElse(null))

	println("rel predictions (training):")
	Eval.AggregateEval(multir, train.value.getOrElse(null))

	if(i % 10 == 0 && i > 0) {
	  println("averaged parameters")
	  multir.computeThetaAverage
	  Eval.useAveragedParameters = true
	  println("test")
	  Eval.AggregateEval(multir, test.value.getOrElse(null))
	  println("train")
	  Eval.AggregateEval(multir, train.value.getOrElse(null))
	  Eval.useAveragedParameters = false
	}

	if(Constants.TIMING) {
	  Utils.Timer.print
	  Utils.Timer.reset
	}
      }
    }        

    /*
     * DNMAR
     */
    if(true) {
      println("evaluating DNMAR")
      val dnmar = new DNMAR(train.value.getOrElse(null))
      for(i <- 0 to 150) {
	println("iteration " + i)
	//      dnmar.trainSimple      = i == 0
	//      dnmar.updatePhi        = i > 0
	
	//dnmar.updateTheta = i <  10 || i % 2 == 1
	//dnmar.updatePhi   = i >= 10 && i % 2 == 0

	//println("updateTheta=\t" + dnmar.updateTheta)
	//println("updatePhi=\t" +   dnmar.updatePhi)

	println("Human annotated evaluation")
	Eval.HumanEval(dnmar, test.value.getOrElse(null), "/home/aritter/dlvm/multir-release/annotations/sentential.txt")

	dnmar.train(1)

	//      println("PHI:")
	//      dnmar.printPhi

	//      println("obs predictions:")
	//      Eval.useObsPredictions = true
	//      Eval.AggregateEval(dnmar, test.value.getOrElse(null))
	println("rel predictions:")
	Eval.useObsPredictions = false
	Eval.AggregateEval(dnmar, test.value.getOrElse(null))

	println("rel predictions (training):")
	Eval.useObsPredictions = false
	Eval.AggregateEval(dnmar, test.value.getOrElse(null))

	println("Human annotated evaluation")
	Eval.HumanEval(dnmar, test.value.getOrElse(null), "/home/aritter/dlvm/multir-release/annotations/sentential.txt")

	if(i % 10 == 0 && i >= 10) {
	  println("averaged parameters")
	  dnmar.computeThetaAverage
	  Eval.useAveragedParameters = true

	  Eval.useObsPredictions = false
	  Eval.AggregateEval(dnmar, test.value.getOrElse(null))
	  //	Eval.useObsPredictions = false
	  //	Eval.AggregateEval(dnmar, test.value.getOrElse(null))
	  println("Human annotated evaluation (averaged)")
	  Eval.HumanEval(dnmar, test.value.getOrElse(null), "/home/aritter/dlvm/multir-release/annotations/sentential.txt")
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
}
