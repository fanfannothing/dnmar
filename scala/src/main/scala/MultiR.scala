package dnmar;

import scalala.scalar._;
import scalala.tensor.::;
import scalala.tensor.mutable._;
import scalala.tensor.dense._;
import scalala.tensor.sparse._;
import scalala.library.Library._;
import scalala.library.Numerics._;
import scalala.library.LinearAlgebra._;
import scalala.library.Statistics._;
import scalala.library.Plotting._;
import scalala.operators.Implicits._;

class MultiR(data:EntityPairData) extends Parameters(data) {
  def train(nIter:Int) = { 
    for(i <- 0 until nIter) {
      println("iteration " + i)
      for(e12 <- 0 until data.data.length) { 
	//Throw out 90% of negative data...
	if(data.data(e12).rel(data.relVocab("NA")) == 0.0 || scala.util.Random.nextDouble < 0.1) {
	  //println("EntityPair " + e12)
	  updateTheta(e12)
	}
      }
    }
    //averageParameters
  }

  def inferHidden(ep:EntityPair):EntityPair = {
    if(Constants.TIMING) {
      Utils.Timer.start("inferHidden")
    }
    //val result = new EntityPair(ep.e1id, ep.e2id, ep.xCond, ep.rel)

    val z      = DenseVector.zeros[Int](ep.xCond.length)
    val zScore = DenseVector.zeros[Double](ep.xCond.length)
    val postZ  = new Array[SparseVector[Double]](ep.xCond.length)
    //val postZ  = new Array[Array[Double]](ep.xCond.length)

    for(i <- 0 until ep.xCond.length) {
      //postZ(i) = MathUtils.LogNormalize((theta * ep.xCond(i)).toArray)
      postZ(i) = theta * ep.xCond(i)

      //TODO: this is kind of a hack... probably need to do what was actually done in the multiR paper...
      val min = postZ(i).min
      postZ(i)(ep.rel :== 0) := postZ(i).min - 1

      z(i)      = postZ(i).argmax
      zScore(i) = postZ(i).max
    }
    if(Constants.DEBUG) {
      println("constrained result.z=" + z.toList.map((r) => data.relVocab(r)))
    }

    val result = new EntityPair(ep.e1id, ep.e2id, ep.xCond, ep.rel, z, zScore)

    if(Constants.TIMING) {
      Utils.Timer.stop("inferHidden")
    }
    result
  }

  def inferAll(ep:EntityPair):EntityPair = {
    if(Constants.TIMING) {
      Utils.Timer.start("inferAll")
    }
    //val result = new EntityPair(ep.e1id, ep.e2id, ep.xCond, DenseVector.zeros[Double](data.nRel).t)

    val z      = DenseVector.zeros[Int](ep.xCond.length)
    val postZ  = new Array[SparseVector[Double]](ep.xCond.length)
    val zScore = DenseVector.zeros[Double](ep.xCond.length)
    val rel    = DenseVector.zeros[Double](data.nRel).t

    for(i <- 0 until ep.xCond.length) {
      postZ(i) = theta * ep.xCond(i)
      postZ(i) = postZ(i) - logSum(postZ(i).toList)	//TODO: does this even make sense?
      z(i) = postZ(i).argmax
      zScore(i) = postZ(i).max

      //Set the aggregate variables
      rel(z(i)) = 1.0

      /*
      if(Constants.DEBUG) {
	val maxFeature = (theta(z(i),::) :* ep.xCond(i).toDense).argmax
	println("maxFeature(" + data.relVocab(z(i)) + ").argmax=" + data.featureVocab(maxFeature))
      }
      */
    }
    if(Constants.DEBUG) {
      println("unconstrained result.z=" + z.toList.map((r) => data.relVocab(r)))
    }

    //TODO: get rid of zScore, replace with postZ...
    val result = new EntityPair(ep.e1id, ep.e2id, ep.xCond, rel, z, zScore)

    if(Constants.TIMING) {
      Utils.Timer.stop("inferAll")
    }
    result
  }
}
