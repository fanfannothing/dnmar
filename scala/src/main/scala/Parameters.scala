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
   * TODO: split phi into different categories rather than having a single large vector
   */
  val phi = SparseVector.zeros[Double](data.entityVocab.size + data.relVocab.size)	//Observation parameters (just 3 things for now - e1, e2, rel)


  //TODO
  def updatePhi(iAll:EntityPair, iHidden:EntityPair) { 
    if(Constants.TIMING) {
      Utils.Timer.start("updatePhi")
    }

    //Update le weights
    //TODO: not quite sure about this...  Probably need to write inference code...
    for(r <- 0 until iAll.rel.length) {
      if(iAll.obs(r) != iHidden.obs(r)) {
	phi(iHidden.e1id)              += iHidden.obs(r)
	phi(iHidden.e2id)              += iHidden.obs(r)
	phi(data.entityVocab.size + r) += iHidden.rel(r)

	phi(iAll.e1id)                 -= iAll.obs(r)
	phi(iAll.e2id)                 -= iAll.obs(r)
	phi(data.entityVocab.size + r) -= iAll.rel(r)
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
