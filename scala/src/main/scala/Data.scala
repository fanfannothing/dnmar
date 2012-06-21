import scalala.scalar._;
import scalala.tensor.::;
import scalala.tensor.mutable._;
import scalala.tensor.dense._;
import scalala.tensor.sparse._;
import scalala.library.Library._;
import scalala.library.LinearAlgebra._;
import scalala.library.Statistics._;
import scalala.library.Plotting._;
import scalala.operators.Implicits._;

class Mention(features:Array[Int]) { 
}

class EntityPair(e1id:Int, e2id:Int, mentions:Array[Mention]) {
}

class Rel(id:Int, instances:Array[EntityPair]) { 
}

class ProtobufData(inFile:String) { 
  val entityVocab  = new Vocab
  val relVocab     = new Vocab
  val featureVocab = new Vocab

  val is = new GZIPInputStream(new BufferedInputStream(new FileInputStream(inFile)))
  var r = Relation.parseDelimitedFrom(is);
  while(r != null) {
    println(r.getRelType())

    r.getSourceGuid
    r.getDestGuid

    r = Relation.parseDelimitedFrom(is)
  }
  sValue  
}
