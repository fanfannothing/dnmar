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
				//Innitialize bias feature for "NA"
  //theta(data.relVocab("NA"), nFeat) = -10.0
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
  val phiPos = SparseVector.zeros[Double](data.entityVocab.size + data.relVocab.size + 1)	//Add space for bias feature...
  val phiNeg = SparseVector.zeros[Double](data.entityVocab.size + data.relVocab.size + 1)	//Add space for bias feature...
  //val phiPos = DenseVector.rand(data.entityVocab.size + data.relVocab.size + 1)	//Observation parameters (just 3 things for now - e1, e2, rel)

  //Innitialize bias features
  phiPos(phiPos.length-1) =  100.0
  phiNeg(phiPos.length-1) =   -5.0

  def updatePhi(iAll:EntityPair, iHidden:EntityPair) { 
    if(Constants.TIMING) {
      Utils.Timer.start("updatePhi")
    }

    assert(iAll.e1id == iHidden.e1id && iAll.e2id == iHidden.e2id, "updatePhi: iAll and iHidden are different...")

    //Update le weights
    val e1id = iHidden.e1id
    val e2id = iHidden.e2id

    //TODO: have some doubts here...
    for(r <- 0 until iAll.rel.length) {
      if(iHidden.obs(r) == 1.0 && iHidden.rel(r) == 1.0) {
	//phiPos(iHidden.e1id)              += 1.0
	//phiPos(iHidden.e2id)              += 1.0
	phiPos(data.entityVocab.size + r) += 1.0
	phiPos(phiPos.length-1)           += 1.0	
      }
      if(iAll.obs(r) == 1.0 && iAll.rel(r) == 1.0) {
	//phiPos(iHidden.e1id)              -= 1.0
	//phiPos(iHidden.e2id)              -= 1.0
	phiPos(data.entityVocab.size + r) -= 1.0
	phiPos(phiPos.length-1)           -= 1.0	
      }

      if(iHidden.obs(r) == 0.0 && iHidden.rel(r) == 1.0) {
	//phiNeg(iHidden.e1id)              += 1.0
	//phiNeg(iHidden.e2id)              += 1.0
	phiNeg(data.entityVocab.size + r) += 1.0
	phiNeg(phiNeg.length-1)           += 1.0
      }
      if(iAll.obs(r) == 0.0 && iAll.rel(r) == 1.0) {
	//phiNeg(iHidden.e1id)              -= 1.0
	//phiNeg(iHidden.e2id)              -= 1.0
	phiNeg(data.entityVocab.size + r) -= 1.0
	phiNeg(phiNeg.length-1)           -= 1.0	
      }
    }    

    if(Constants.TIMING) {
      Utils.Timer.stop("updatePhi")
    }
  }

  def printPhi {
    println("PhiPos************************")
    println("bias\t" + phiPos(phiPos.length-1))
    for(i <- (0 until phiPos.length).toList.sortBy((j) => -phiPos(j)).slice(0,10)) {
      if(i < data.entityVocab.size) {
	println(data.entityVocab(i) + "\t" + phiPos(i))
      } else if(i < data.entityVocab.size + data.relVocab.size) {
	println(data.relVocab(i - data.entityVocab.size) + "\t" + phiPos(i))
      }
    }
    println("PhiNeg************************")
    println("bias\t" + phiNeg(phiNeg.length-1))
    for(i <- (0 until phiNeg.length).toList.sortBy((j) => -phiNeg(j)).slice(0,10)) {
      if(i < data.entityVocab.size) {
	println(data.entityVocab(i) + "\t" + phiNeg(i))
      } else if(i < data.entityVocab.size + data.relVocab.size) {
	println(data.relVocab(i - data.entityVocab.size) + "\t" + phiNeg(i))
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
