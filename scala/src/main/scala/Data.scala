package dnmar;

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

import java.util.zip.GZIPInputStream
import java.io.FileInputStream
import java.io.BufferedInputStream
import java.io.InputStream

import cc.factorie.protobuf.DocumentProtos.Relation
import cc.factorie.protobuf.DocumentProtos.Relation.RelationMentionRef

/**************************************************************************
 * EntityPair
 * Stores all observed and hidden variables associated with a pair
 * of entities (e1id,e2id)
 **************************************************************************
 */
class EntityPair(val e1id:Int, val e2id:Int, val xCond:Array[SparseVectorCol[Double]], val rel:DenseVectorRow[Double]) {
//class EntityPair(val e1id:Int, val e2id:Int, val xCond:Array[DenseVectorCol[Double]], val rel:DenseVector[Double]) {
  val obs = rel.toDense							//Which variables are observed, just copy
  val z   = DenseVector.randi(rel.length, xCond.length)			//Sentence level classification
}

abstract class EntityPairData {
  val data:Array[EntityPair]
  val nRel:Int
  val nFeat:Int

  val entityVocab:Vocab
  val relVocab:Vocab
  val featureVocab:Vocab
}

//Class to read and manage data from google protobuf file format
class ProtobufData(inFile:String, evoc:Vocab, rvoc:Vocab, fvoc:Vocab) extends EntityPairData { 
  def this(inFile:String) = this(inFile, null, null, null)

  val entityVocab  = if(evoc != null) { evoc } else { new Vocab }
  val relVocab     = if(rvoc != null) { rvoc } else { new Vocab }
  val featureVocab = if(fvoc != null) { fvoc } else { new Vocab }

  var is = new GZIPInputStream(new BufferedInputStream(new FileInputStream(inFile)))
  var r = Relation.parseDelimitedFrom(is);
  var nEntityPairs = 0

  //First pass: figure out vocabulary sizes
  if(featureVocab.size == 0) {
    while(r != null) {
      //println(r.getRelType())

      entityVocab(r.getSourceGuid)
      entityVocab(r.getDestGuid)

      for(rel <- r.getRelType.split(",")) {
	relVocab(rel)
      }

      for(i <- 0 until r.getMentionCount) {
	val m = r.getMention(i)
	for(j <- 0 until m.getFeatureCount) {
	  //println(m.getFeature(j))
	  featureVocab(m.getFeature(j))
	}
      }

      nEntityPairs += 1
      r = Relation.parseDelimitedFrom(is)
    }
  }

  println("f: " + featureVocab.size)
  println("e: " + entityVocab.size)
  println("r: " + relVocab.size)

  val nRel  = relVocab.size
  val nFeat = featureVocab.size

  val data = new Array[EntityPair](nEntityPairs)

  //Second pass: Populate the data structures
  is = new GZIPInputStream(new BufferedInputStream(new FileInputStream(inFile)))
  r = Relation.parseDelimitedFrom(is);
  nEntityPairs = 0
  while(r != null) {
    //println(r.getRelType())

    val e1 = entityVocab(r.getSourceGuid)
    val e2 = entityVocab(r.getDestGuid)

    val relations = DenseVector.zeros[Double](relVocab.size)
    for(rel <- r.getRelType.split(",")) {
      val r = relVocab(rel)
      if(r >= 0) {
	relations(r) = 1.0
      }
    }

    val mentions = new Array[SparseVectorCol[Double]](r.getMentionCount)
    //val mentions = new Array[DenseVectorCol[Double]](r.getMentionCount)
    for(i <- 0 until r.getMentionCount) {
      mentions(i) = SparseVector.zeros[Double](featureVocab.size + 1)
      mentions(i)(featureVocab.size) = 1.0	//Bias feature
      val m = r.getMention(i)
      for(j <- 0 until m.getFeatureCount) {
	val f = featureVocab(m.getFeature(j))
	if(f >= 0) {
	  mentions(i)(f) = 1.0
	}
      }
    }
    
    data(nEntityPairs) = new EntityPair(e1, e2, mentions, relations.t)

    nEntityPairs += 1
    r = Relation.parseDelimitedFrom(is)
  }
}
