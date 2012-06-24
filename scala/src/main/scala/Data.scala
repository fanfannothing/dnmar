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
class ProtobufData(inFile:String) extends EntityPairData { 
  val entityVocab  = new Vocab
  val relVocab     = new Vocab
  val featureVocab = new Vocab

  var is = new GZIPInputStream(new BufferedInputStream(new FileInputStream(inFile)))
  var r = Relation.parseDelimitedFrom(is);
  var nEntityPairs = 0
  //First pass: figure out vocabulary sizes
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

  println("f: " + featureVocab.VocabSize)
  println("e: " + entityVocab.VocabSize)
  println("r: " + relVocab.VocabSize)

  val nRel  = relVocab.VocabSize
  val nFeat = featureVocab.VocabSize

  val data = new Array[EntityPair](nEntityPairs)

  //Second pass: Populate the data structures
  is = new GZIPInputStream(new BufferedInputStream(new FileInputStream(inFile)))
  r = Relation.parseDelimitedFrom(is);
  nEntityPairs = 0
  while(r != null) {
    //println(r.getRelType())

    val e1 = entityVocab(r.getSourceGuid)
    val e2 = entityVocab(r.getDestGuid)

    val relations = DenseVector.zeros[Double](relVocab.VocabSize)
    for(rel <- r.getRelType.split(",")) {
      relations(relVocab(rel)) = 1.0
    }

    val mentions = new Array[SparseVectorCol[Double]](r.getMentionCount)
    //val mentions = new Array[DenseVectorCol[Double]](r.getMentionCount)
    for(i <- 0 until r.getMentionCount) {
      mentions(i) = SparseVector.zeros[Double](featureVocab.VocabSize + 1)
      //mentions(i) = DenseVector.zeros[Double](featureVocab.VocabSize + 1)
      mentions(i)(featureVocab.VocabSize) = 1.0	//Bias feature
      val m = r.getMention(i)
      for(j <- 0 until m.getFeatureCount) {
	mentions(i)(featureVocab(m.getFeature(j))) = 1.0
      }
    }
    
    data(nEntityPairs) = new EntityPair(e1, e2, mentions, relations.t)

    nEntityPairs += 1
    r = Relation.parseDelimitedFrom(is)
  }
}
