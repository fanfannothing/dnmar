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
  val thetaAveraged = new Array[SparseVector[Double]](nRel)
  //val thetaAveraged = DenseMatrix.zeros[Double](nRel,nFeat+1)
  val sparseTheta   = new Array[SparseVector[Double]](nRel)
  for(i <- 0 until nRel) {
    sparseTheta(i)   = SparseVector.zeros[Double](nFeat+1)
    thetaAveraged(i) = SparseVector.zeros[Double](nFeat+1)
  }
  var thetaAvCount  = 0.0

  def averageTheta { 
    for(r <- 0 until nRel) { 
      theta(r,::) := (thetaAveraged(r) :/ thetaAvCount)
    }
  }

  def updateThetaAverage {
    if(Constants.TIMING) {
      Utils.Timer.start("updateThetaAverage")
    }

    for(r <- 0 until nRel) {
      //thetaAveraged(r) += theta(r,::)
      thetaAveraged(r) += sparseTheta(r)
    }
    thetaAvCount += 1.0

    if(Constants.TIMING) {
      Utils.Timer.stop("updateThetaAverage")
    }
  }

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

	//sparseTheta(iHidden.z(m)) :+= iHidden.xCond(m)
	//sparseTheta(iAll.z(m))    :-= iAll.xCond(m)
      }
    }
//    if(scala.util.Random.nextDouble < 0.01) {	//Update average on 1% of weights (for efficiency...)
//      updateThetaAverage
//    }

    if(Constants.TIMING) {
      Utils.Timer.stop("updateTheta")
    }
    
    //Compute conditional likelihood?
  }

  /*********************************************************************
   * PHI
   *********************************************************************
   */
  val phi = DenseVector.zeros[Double](3)	//Observation parameters (just 3 parameters for now - e1, e2, rel)

  /*********************************************************************
   * Inference (Must be implemented in implementation class)
   *********************************************************************
   */
  def inferHidden(ep:EntityPair):EntityPair

  def inferAll(ep:EntityPair):EntityPair
}
