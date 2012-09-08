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
				      train.value.getOrElse(null).featureVocab.lock)
  }

  def main(args: Array[String]) {
    try { 
      parser.parse(args)
    }
    catch { 
      case e: ArgotUsageException => println(e.message)
    }

    val dnmar = new DNMAR(train.value.getOrElse(null))
    //val dnmar = new DNMAR_greedy(train.value.getOrElse(null))
    //val dnmar = new MultiR(train.value.getOrElse(null))

    for(i <- 0 until 100) {
      println("iteration " + i)
      dnmar.trainSimple      = i < 10
      //dnmar.updatePhi        = i >= 10
      
      dnmar.updateTheta = i <  10 || i % 2 == 1
      dnmar.updatePhi   = i >= 10 && i % 2 == 0

      println("updateTheta=\t" + dnmar.updateTheta)
      println("updatePhi=\t" +   dnmar.updatePhi)

      dnmar.train(1)
      Eval.useObsPredictions = i >= 10
      //Eval.useObsPredictions = true
      Eval.AggregateEval(dnmar, test.value.getOrElse(null))
      Eval.useObsPredictions = false
      Eval.AggregateEval(dnmar, test.value.getOrElse(null))
    }
    
    if(Constants.TIMING) {
      Utils.Timer.print
    }
  }
}
