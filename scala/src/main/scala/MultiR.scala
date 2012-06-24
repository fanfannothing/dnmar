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

class MultiR(data:EntityPairData) extends Parameters(data) {
  val relRange = DenseVector((0 until nRel).toArray)
  def inferHidden(ep:EntityPair):EntityPair = {
    if(Constants.TIMING) {
      Utils.Timer.start("inferHidden")
    }
    val result = new EntityPair(ep.e1id, ep.e2id, ep.xCond, ep.rel)
    val postZ = new Array[SparseVector[Double]](result.xCond.length)

    for(i <- 0 until result.xCond.length) {
      postZ(i) = theta * result.xCond(i)

      //TODO: this is kind of a hack... probably need to do what was actually done in the multiR paper...
      val min = postZ(i).min
      postZ(i)(ep.rel :== 0) := postZ(i).min - 1

      result.z(i) = postZ(i).argmax
    }
    if(Constants.DEBUG) {
      println("constrained result.z=" + result.z.toList.map((r) => data.relVocab(r)))
    }

    if(Constants.TIMING) {
      Utils.Timer.stop("inferHidden")
    }
    result
  }

  def inferAll(ep:EntityPair):EntityPair = {
    if(Constants.TIMING) {
      Utils.Timer.start("inferAll")
    }
    val result = new EntityPair(ep.e1id, ep.e2id, ep.xCond, ep.rel)
    val postZ = new Array[SparseVector[Double]](result.xCond.length)
    for(i <- 0 until result.xCond.length) {
      postZ(i) = theta * result.xCond(i)
      result.z(i) = postZ(i).argmax

      if(Constants.DEBUG) {
	val maxFeature = (theta(result.z(i),::) :* result.xCond(i).toDense).argmax
	println("maxFeature(" + data.relVocab(result.z(i)) + ").argmax=" + data.featureVocab(maxFeature))
      }
    }
    if(Constants.DEBUG) {
      println("unconstrained result.z=" + result.z.toList.map((r) => data.relVocab(r)))
    }

    if(Constants.TIMING) {
      Utils.Timer.stop("inferAll")
    }
    result
  }
}
