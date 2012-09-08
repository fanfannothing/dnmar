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

abstract class Parameters(data:EntityPairData) {
  val nRel  = data.nRel
  val nFeat = data.nFeat

  /*********************************************************************
   * THETA
   *********************************************************************
   */
  val theta = DenseMatrix.zeros[Double](nRel,nFeat+1)
  //val theta = DenseMatrix.rand(nRel,nFeat+1)

  def updateTheta(iAll:EntityPair, iHidden:EntityPair) {
    if(Constants.TIMING) {
      Utils.Timer.start("updateTheta")
    }

    //Update le weights
    for(m <- 0 until iAll.xCond.length) {
      if(iAll.z(m) != iHidden.z(m)) {
	theta(iHidden.z(m),::)    :+= iHidden.xCond(m)
	theta(iAll.z(m),   ::)    :-= iAll.xCond(m)
      }
    }

    if(Constants.TIMING) {
      Utils.Timer.stop("updateTheta")
    }
    
    //Compute conditional likelihood?
 }

  /*********************************************************************
   * PHI
   *********************************************************************
   */
  /*
   * TODO: split phi into different categories rather than having a single large vector?
   */
  val phi = SparseVector.zeros[Double](data.entityVocab.size + data.relVocab.size + 1)	//Add space for bias feature...
  //val phi = DenseVector.rand(data.entityVocab.size + data.relVocab.size + 1)	//Observation parameters (just 3 things for now - e1, e2, rel)


  //TODO
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
	  phi(iHidden.e1id)              += 1.0
	  phi(iHidden.e2id)              += 1.0
	  phi(data.entityVocab.size + r) += 1.0
	  phi(phi.length-1)              += 1.0
	}

	if(iAll.postObs(r) >= 0.5) {
	  phi(iAll.e1id)                 -= 1.0
	  phi(iAll.e2id)                 -= 1.0
	  phi(data.entityVocab.size + r) -= 1.0
	  phi(phi.length-1)              -= 1.0
	}
      }
    }    

    if(Constants.TIMING) {
      Utils.Timer.stop("updatePhi")
    }
  }
  

  /*********************************************************************
   * Inference (Must be implemented in implementation class)
   *********************************************************************
   */
  def inferHidden(ep:EntityPair):EntityPair

  def inferAll(ep:EntityPair):EntityPair
}
