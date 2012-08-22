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

import scala.collection.mutable.ListBuffer
import scala.util.Random

class AllVariablesHypothesis(postZ:DenseMatrix[Double], postObs:DenseVector[Double], zPartial:ListBuffer[Int], obs:Array[Double], sPartial:Double) extends Hypothesis {
  val score = sPartial
  val z = zPartial

  def sucessors:Array[Hypothesis] = {
    val result = new ListBuffer[Hypothesis]

    for(rel <- 0 until postZ.numCols) {
      //Add in factor for next item
      var newScore = score + log(postZ(z.length, rel))	      //Use log score
      val newZ = z + rel
      var newObs = obs

      //Did we change one of the observation variables?
      if(newObs(rel) == 1.0) {
	newObs = obs.clone
	newObs(rel) = 0.0
	val pObs  = postObs(rel)
	val pNobs = 1.0 - postObs(rel)
	newScore = newScore - log(pObs) + log(pNobs)
      }
      
      result += new AllVariablesHypothesis(postZ, postObs, newZ, newObs, newScore)
    }
    return result.toArray
  }
}

class HiddenVariablesHypothesis(postZ:DenseMatrix[Double], z:ListBuffer[Int]) extends Hypothesis { 
  def score:Double = { 
    return 1.0
  }

  def sucessors:Array[Hypothesis] = { 
    val result = new ListBuffer[Hypothesis]
    return result.toArray
  }
}

class DNMAR(data:EntityPairData) extends Parameters(data) {
  //Randomly permute the training data
  //Throw out about 90% of negative data...
  val training = Random.shuffle((0 until data.data.length).toList).filter((e12) => data.data(e12).rel(data.relVocab("NA")) == 0.0 || scala.util.Random.nextDouble < 0.2)
  //val training = Random.shuffle((0 until data.data.length).toList).filter((e12) => data.data(e12).rel(data.relVocab("NA")) == 0.0 || scala.util.Random.nextDouble < 0.1)

  def train(nIter:Int) = { 
    for(i <- 0 until nIter) {
      //println("iteration " + i)
      for(e12 <- training) {
	//Run le inference
	val iAll    = inferAll(data.data(e12))
	val iHidden = inferHidden(data.data(e12))
	updateTheta(iAll, iHidden)
	updatePhi(iAll, iHidden)
      }
    }
  }

  def inferHidden(ep:EntityPair):EntityPair = {
    if(Constants.TIMING) {
      Utils.Timer.start("inferHidden")
    }
    //val result = new EntityPair(ep.e1id, ep.e2id, ep.xCond, ep.rel)

    val z      = DenseVector.zeros[Int](ep.xCond.length)
    val zScore = DenseVector.zeros[Double](ep.xCond.length)
    //val postZ  = new Array[DenseVector[Double]](ep.xCond.length)
    val postZ  = DenseMatrix.zeros[Double](ep.xCond.length, data.nRel)

    for(i <- 0 until ep.xCond.length) {
      //postZ(i) = MathUtils.LogNormalize((theta * ep.xCond(i)).toArray)
      postZ(i,::) := exp((theta * ep.xCond(i)).toDense)

      //TODO: Create AllVariablesHypothesis and do beam search

      //TODO: this is kind of a hack... probably need to do what was actually done in the multiR paper...
      //postZ(i)(ep.rel :== 0) := Double.MinValue

      z(i)      = postZ(i,::).argmax
      zScore(i) = postZ(i,::).max

      ep.rel(z(i)) = 1.0
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
    //val postZ  = new Array[DenseVector[Double]](ep.xCond.length)
    val postZ  = DenseMatrix.zeros[Double](ep.xCond.length, data.nRel)
    val zScore = DenseVector.zeros[Double](ep.xCond.length)
    val rel    = DenseVector.zeros[Double](data.nRel).t

    for(i <- 0 until ep.xCond.length) {
      postZ(i,::) := exp((theta * ep.xCond(i)).toDense)

      z(i) = postZ(i,::).argmax
      zScore(i) = postZ(i,::).max

      //Set the aggregate variables
      rel(z(i)) = 1.0
    }

    //predict observation variables
    val postObs = DenseVector.zeros[Double](data.nRel)
    for(r <- 0 until ep.rel.length) {
      if(ep.obs(r) == 0.0) {
	var s = 0.0
	s += phi(ep.e1id)
	s += phi(ep.e2id)
	s += phi(data.entityVocab.size + r)
	val p = 1.0 / (1.0 + exp(s))
	postObs(r) = p

	if(p > 0.5) {
	  ep.obs(r) = 1.0
	}
      }
    }

    //TODO: Create AllVariablesHypothesis and do beam search
    val bs = new BeamSearch(new AllVariablesHypothesis(postZ, postObs, new ListBuffer[Int], Array.fill[Double](data.nRel)(0.0), 0.0), 20);
    
    while(bs.Head.asInstanceOf[AllVariablesHypothesis].z.length < ep.xCond.length) {
      bs.UpdateQ
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
