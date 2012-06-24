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

abstract class Parameters(data:EntityPairData) {
  val nRel  = data.nRel
  val nFeat = data.nFeat

  val theta = DenseMatrix.zeros[Double](nRel,nFeat+1)
  val phi = DenseVector.zeros[Double](3)	//Observation parameters (just 3 parameters for now - e1, e2, rel)

  def inferHidden(ep:EntityPair):EntityPair

  def inferAll(ep:EntityPair):EntityPair

  def updateTheta(i:Int) {
    if(Constants.TIMING) {
      Utils.Timer.start("updateTheta")
    }

    val ep = data.data(i)

    //Run inference
    val iAll    = inferAll(ep)

    //TODO: check if we're violating any constraints and skip the rest if we're not (for efficiency)?

    val iHidden = inferHidden(ep)

    //Update the weights
    for(r <- 0 until nRel) {
      theta(r,::) :+= thetaExpectation(iHidden, r) - thetaExpectation(iAll, r)

      /*
      if(Constants.DEBUG) {
	println("theta(" + data.relVocab(r) + ").max=" + theta(r,::).max)
	println("theta(" + data.relVocab(r) + ").argmax=" + data.featureVocab(theta(r,::).argmax))
      }
      */
    }

    if(Constants.TIMING) {
      Utils.Timer.stop("updateTheta")
    }
    
    //Compute conditional likelihood?
  }

  def thetaExpectation(ep:EntityPair, rel:Int):SparseVector[Double] = {
    var result = SparseVector.zeros[Double](nFeat+1)
    for(k <- 0 until ep.xCond.length) {
      if(ep.z(k) == rel) {
	result :+= ep.xCond(k)
      }
    }
    result
  }
}
