import cc.factorie.protobuf.DocumentProtos.Relation
import cc.factorie.protobuf.DocumentProtos.Relation.RelationMentionRef

import java.util.zip.GZIPInputStream
import java.io.FileInputStream
import java.io.BufferedInputStream
import java.io.InputStream

object Hi {
  def main(args: Array[String]) {
    val is = new GZIPInputStream(new BufferedInputStream(new FileInputStream(args(0))))

    var r = Relation.parseDelimitedFrom(is)
    while(r != null) {
      println(r.getRelType())
      r = Relation.parseDelimitedFrom(is)
    }
    
    println("Hi!")
  }
}
