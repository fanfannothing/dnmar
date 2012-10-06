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

import scala.util.Random

abstract class Parameters(val data:EntityPairData) {
  val nRel  = data.nRel
  val nFeat = data.nFeat

  /*********************************************************************
   * THETA
   *********************************************************************
   */
  val theta         = DenseMatrix.zeros[Double](nRel,nFeat+1)
  val theta_sum     = DenseMatrix.zeros[Double](nRel,nFeat+1)

  var theta_average = DenseMatrix.zeros[Double](nRel,nFeat+1)

  var nUpdates = 1.0

  def computeThetaAverage {
    if(Constants.TIMING) {
      Utils.Timer.start("computeThetaAverage")
    }

    theta_average = theta - (theta_sum / nUpdates)

    if(Constants.TIMING) {
      Utils.Timer.stop("computeThetaAverage")
    }
  }

  def updateTheta(iAll:EntityPair, iHidden:EntityPair) {
    if(Constants.TIMING) {
      Utils.Timer.start("updateTheta")
    }

    //Update le weights
    for(m <- 0 until iAll.features.length) {
      if(iAll.z(m) != iHidden.z(m)) {
	theta(iHidden.z(m),::)     :+= iHidden.features(m)
	theta(iAll.z(m),   ::)     :-= iAll.features(m)

	theta_sum(iHidden.z(m),::) :+= (nUpdates :* iHidden.features(m))
	theta_sum(iAll.z(m),   ::) :-= (nUpdates :* iAll.features(m))
      }
    }

    nUpdates += 1.0
    //Compute conditional likelihood?

    if(Constants.TIMING) {
      Utils.Timer.stop("updateTheta")
    }
 }

  /*********************************************************************
   * PHI
   *********************************************************************
   */
  val phi = SparseVector.zeros[Double](data.entityVocab.size + data.relVocab.size + 1)	//Add space for bias feature...
  //val phi = DenseVector.rand(data.entityVocab.size + data.relVocab.size + 1)	//Observation parameters (just 3 things for now - e1, e2, rel)

  //Innitialize bias feature
  phi(phi.length-1) = 100

  def updatePhi(iAll:EntityPair, iHidden:EntityPair) { 
    if(Constants.TIMING) {
      Utils.Timer.start("updatePhi")
    }

    assert(iAll.e1id == iHidden.e1id && iAll.e2id == iHidden.e2id, "updatePhi: iAll and iHidden are different...")

    //Update le weights
    val e1id = iHidden.e1id
    val e2id = iHidden.e2id

    for(r <- 0 until iAll.rel.length) {
      if(iAll.rel(r) == 1.0) {						//If we think this fact is true, then update parameters...
	if(iHidden.obs(r) >= 0.5) {
	  //phi(iHidden.e1id)              += 1.0
	  //phi(iHidden.e2id)              += 1.0
	  phi(data.entityVocab.size + r) += 1.0
	  phi(phi.length-1)              += 1.0
	}

	if(iAll.obs(r) >= 0.5) {
	  //phi(iAll.e1id)                 -= 1.0
	  //phi(iAll.e2id)                 -= 1.0
	  phi(data.entityVocab.size + r) -= 1.0
	  phi(phi.length-1)              -= 1.0
	}
      }
    }    

    if(Constants.TIMING) {
      Utils.Timer.stop("updatePhi")
    }
  }

  def printPhi {
    println("bias\t" + phi(phi.length-1))
    for(i <- (0 until phi.length).toList.sortBy((j) => -phi(j)).slice(0,10)) {
      if(i < data.entityVocab.size) {
	println(data.entityVocab(i) + "\t" + phi(i))
      } else if(i < data.entityVocab.size + data.relVocab.size) {
	println(data.relVocab(i - data.entityVocab.size) + "\t" + phi(i))
      }
    }
  }
  

  /*********************************************************************
   * Inference (Must be in implementation class)
   *********************************************************************
   */
  def inferHidden(ep:EntityPair):EntityPair

  def inferAll(ep:EntityPair):EntityPair
  def inferAll(ep:EntityPair, useAverage:Boolean):EntityPair
  def train(iterations:Int)
}
