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
class EntityPair(val e1id:Int, val e2id:Int, val features:Array[SparseVectorCol[Double]], val rel:DenseVectorRow[Double], val z:DenseVector[Int], val zScore:DenseVector[Double], val obs:DenseVector[Double], val sentences:Array[String]) {
  def this(e1id:Int, e2id:Int, features:Array[SparseVectorCol[Double]], rel:DenseVectorRow[Double], z:DenseVector[Int], zScore:DenseVector[Double], obs:DenseVector[Double]) = this(e1id, e2id, features, rel, z, zScore, obs, null)
  def this(e1id:Int, e2id:Int, features:Array[SparseVectorCol[Double]], rel:DenseVectorRow[Double]) = this(e1id, e2id, features, rel, null, null, rel.toDense, null)
  def this(e1id:Int, e2id:Int, features:Array[SparseVectorCol[Double]], rel:DenseVectorRow[Double], sentences:Array[String]) = this(e1id, e2id, features, rel, null, null, rel.toDense, sentences)
  def this(e1id:Int, e2id:Int, features:Array[SparseVectorCol[Double]], rel:DenseVectorRow[Double], z:DenseVector[Int], zScore:DenseVector[Double]) = this(e1id, e2id, features, rel, z, zScore, rel.toDense, null)
  //val obs = rel.toDense							
  var postObs:DenseVector[Double] = null
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
class ProtobufData(inFile:String, evoc:Vocab, rvoc:Vocab, fvoc:Vocab, readSentences:Boolean) extends EntityPairData { 
  def this(inFile:String, readSentences:Boolean) = this(inFile, null, null, null, readSentences)
  def this(inFile:String) = this(inFile, null, null, null, false)

  var newVocab = true
  val entityVocab  = if(evoc != null) { evoc } else { new Vocab }
  val relVocab     = if(rvoc != null) { rvoc } else { new Vocab }
  val featureVocab = if(fvoc != null) { newVocab=false; fvoc } else { new Vocab }

  var is = new GZIPInputStream(new BufferedInputStream(new FileInputStream(inFile)))
  var r = Relation.parseDelimitedFrom(is);
  var nEntityPairs = 0

  //First pass: figure out vocabulary sizes
  while(r != null) {
    entityVocab(r.getSourceGuid)
    entityVocab(r.getDestGuid)

    if(newVocab) {
      for(rel <- r.getRelType.split(",")) {
	relVocab(rel)
      }
      for(i <- 0 until r.getMentionCount) {
	val m = r.getMention(i)
	for(j <- 0 until m.getFeatureCount) {
	  featureVocab(m.getFeature(j))
	}
      }
    }

    nEntityPairs += 1
    r = Relation.parseDelimitedFrom(is)
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

    val mentions  = new Array[SparseVectorCol[Double]](r.getMentionCount)
    val sentences = new Array[String](r.getMentionCount)
    //val mentions = new Array[DenseVectorCol[Double]](r.getMentionCount)
    for(i <- 0 until r.getMentionCount) {
      var nFeatures = 0.0
      mentions(i) = SparseVector.zeros[Double](featureVocab.size + 1)
      mentions(i)(featureVocab.size) = 1.0	//Bias feature
      val m = r.getMention(i)
      sentences(i) = m.getSentence
      //println(m.getSentence)
      for(j <- 0 until m.getFeatureCount) {
	val f = featureVocab(m.getFeature(j))
	if(f >= 0) {
	  mentions(i)(f) = 1.0
	  nFeatures += 1.0
	}
      }
    }

    if(readSentences) {
      data(nEntityPairs) = new EntityPair(e1, e2, mentions, relations.t, sentences)
    } else {
      data(nEntityPairs) = new EntityPair(e1, e2, mentions, relations.t)
    }

    nEntityPairs += 1
    r = Relation.parseDelimitedFrom(is)
  }
}
