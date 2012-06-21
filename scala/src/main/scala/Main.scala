package dnamr;

import java.util.zip.GZIPInputStream
import java.io.FileInputStream
import java.io.BufferedInputStream
import java.io.InputStream

import cc.factorie.protobuf.DocumentProtos.Relation
import cc.factorie.protobuf.DocumentProtos.Relation.RelationMentionRef

import org.clapper.argot._

object Main {
  import ArgotConverters._
  
  val parser = new ArgotParser(
    "DNMAR",
    preUsage=Some("DNMAR: Version 0.1. Copyright (c) " +
                  "2012, Alan Ritter.")
  )

  val trainProto = parser.option[String](List("trainProto"), "n", "Training data (in google protobuf format).") { 
    (sValue, opt) =>
    val is = new GZIPInputStream(new BufferedInputStream(new FileInputStream(sValue)))
    var r = Relation.parseDelimitedFrom(is);
    while(r != null) {
      println(r.getRelType())
      r = Relation.parseDelimitedFrom(is)
    }
    sValue
  }

  val testProto  = parser.option[String](List("testProto"), "n", "Test data (in google protobuf format).") {
    (sValue, opt) =>
    val is = new GZIPInputStream(new BufferedInputStream(new FileInputStream(sValue)))
    var r = Relation.parseDelimitedFrom(is);
    while(r != null) {
      println(r.getRelType())
      r = Relation.parseDelimitedFrom(is)
    }
    sValue
  }

  def main(args: Array[String]) {
    try { 
      parser.parse(args)
    }
    catch { 
      case e: ArgotUsageException => println(e.message)
    }

  }
}
