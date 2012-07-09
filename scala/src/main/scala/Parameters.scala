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
  val theta         = DenseMatrix.zeros[Double](nRel,nFeat+1)

  def updateTheta(i:Int) {
    if(Constants.TIMING) {
      Utils.Timer.start("updateTheta")
    }

    val ep = data.data(i)

    //Run le inference
    val iAll    = inferAll(ep)
    val iHidden = inferHidden(ep)

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
  val phi = SparseVector.zeros[Double](data.entityVocab.size + data.relVocab.size + 1)	//Observation parameters (just 3 parameters for now - e1, e2, rel)

  def updatePhi(i:Int) { 
    //TODO
  }
  

  /*********************************************************************
   * Inference (Must be implemented in implementation class)
   *********************************************************************
   */
  def inferHidden(ep:EntityPair):EntityPair

  def inferAll(ep:EntityPair):EntityPair
}
