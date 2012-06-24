package dnmar;

import java.util.zip.GZIPInputStream
import java.io.FileInputStream
import java.io.BufferedInputStream
import java.io.InputStream

import cc.factorie.protobuf.DocumentProtos.Relation
import cc.factorie.protobuf.DocumentProtos.Relation.RelationMentionRef

import org.clapper.argot._

object Constants {
  val DEBUG = false
  val TIMING = true
}

object Main {
  import ArgotConverters._
  
  val parser = new ArgotParser(
    "DNMAR",
    preUsage=Some("DNMAR: Version 0.1. Copyright (c) " +
                  "2012, Alan Ritter.")
  )

  val train = parser.option[ProtobufData](List("trainProto"), "n", "Training data (in google protobuf format).") { 
    (sValue, opt) => new ProtobufData(sValue)
  }

  val test  = parser.option[ProtobufData](List("testProto"), "n", "Test data (in google protobuf format).") {
    (sValue, opt) => new ProtobufData(sValue)
  }

  def main(args: Array[String]) {
    try { 
      parser.parse(args)
    }
    catch { 
      case e: ArgotUsageException => println(e.message)
    }

    val p = new MultiR(train.value.getOrElse(null))
    
    //for(i <- 0 until train.value.getOrElse(null).data.length) {
    for(i <- 0 until 100) {
      //val j = scala.util.Random.nextInt(train.value.getOrElse(null).data.length)
      val j = i
      println("Entity Pair " + j)
      p.updateTheta(j)
    }
    
    if(Constants.TIMING) {
      Utils.Timer.print
    }
  }
}
