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

//TODO: should predict MAP values for both aggregate extraction variables and observation variables
//TODO: recursively stored score shouldn't include these factors...
class AllVariablesHypothesis(postZ:DenseMatrix[Double], postObs:DenseVector[Double], zPartial:ListBuffer[Int], val rPartial:Array[Double], var sPartial:Double, var score:Double) extends Hypothesis {
  val z = zPartial

  //println(z.length + "\t" + score)

  def obs:DenseVector[Double] = {
    val result = DenseVector.zeros[Double](postZ.numCols)
    //compute MAP observation variables based on rPartial
    for(rel <- 0 until postZ.numCols) {
      if(rPartial(rel) == 1.0) {
        if(postObs(rel) > 0.5) {
          result(rel) = 1.0
        }
      }
    }
    result
  }

  def sucessors:Array[Hypothesis] = {
    val result = new ListBuffer[Hypothesis]

    for(rel <- 0 until postZ.numCols) {

      val newZ = z.clone + rel
      var newRpartial = rPartial.clone
      //TODO: probably need to use a log score here...
      //var newSpartial = sPartial * postZ(z.length, rel)
      var newSpartial = sPartial + math.log(postZ(z.length, rel))
      var newScore = newSpartial

      newRpartial(rel) = 1.0

      for(rel <- 0 until postZ.numCols) {
        if(newRpartial(rel) == 1.0) {
          //newScore *= math.max(postObs(rel), 1-postObs(rel))
          newScore += math.log(math.max(postObs(rel), 1-postObs(rel)))
        }
      }

      for(i <- newZ.length until postZ.numRows) {
        //newScore *= postZ(i,::).max
        newScore += math.log(postZ(i,::).max)
      }

      //println("newScore =" + newScore.toString)
      result += new AllVariablesHypothesis(postZ, postObs, newZ, newRpartial, newSpartial, newScore)
    }
    return result.toArray
  }
}

//TODO: need to actually implement this part
//TODO: should take database into account
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
  //Throw out X% of negative data...
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
      for(r <- 0 until ep.rel.length) {
        if(ep.rel(r) == 0.0) {
          postZ(i,r) = Double.MinValue          
        }
      }

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

    val z      = DenseVector.zeros[Int](ep.xCond.length)
    val postZ  = DenseMatrix.zeros[Double](ep.xCond.length, data.nRel)
    val zScore = DenseVector.zeros[Double](ep.xCond.length)
    val rel    = DenseVector.zeros[Double](data.nRel).t

    for(i <- 0 until ep.xCond.length) {
      postZ(i,::) := exp((theta * ep.xCond(i)).toDense)
      postZ(i,::) /= postZ(i,::).sum        //normalize

      //z(i) = postZ(i,::).argmax
      zScore(i) = postZ(i,::).max

      //Set the aggregate variables
      //rel(z(i)) = 1.0
    }

    //predict observation variables
    val postObs = DenseVector.zeros[Double](data.nRel)
    for(r <- 0 until data.nRel) {
      var s = 0.0
      s += phi(ep.e1id)
      s += phi(ep.e2id)
      s += phi(data.entityVocab.size + r)
      val p = 1.0 / (1.0 + exp(s))
      postObs(r) = p

      if(p > 0.5) {
	ep.obs(r) = 1.0
      } else {
	ep.obs(r) = 0.0
      }
    }

    //val bs = new BeamSearch(new AllVariablesHypothesis(postZ, postObs, new ListBuffer[Int], Array.fill[Double](data.nRel)(0.0), 1.0, 1.0), 10);
    val bs = new BeamSearch(new AllVariablesHypothesis(postZ, postObs, new ListBuffer[Int], Array.fill[Double](data.nRel)(0.0), 0.0, 0.0), 10);
    
    while(bs.Head.asInstanceOf[AllVariablesHypothesis].z.length < ep.xCond.length) {
      //println(bs.Head.asInstanceOf[AllVariablesHypothesis].z.length)
      bs.UpdateQ
    }

    if(Constants.DEBUG) {
      println("unconstrained result.z=" + z.toList.map((r) => data.relVocab(r)))
    }

    //TODO: a bit of reorganizing/refactoring here maybe...
    for(r <- bs.Head.asInstanceOf[AllVariablesHypothesis].z.toArray) {
      rel(r) = 1.0
    }

    /* Some Debugging code...
     * /
    for(r <- 0 until ep.rel.length) {
      if(rel(r) == 1.0) {
        println(rel(r) + "\t" + data.entityVocab(ep.e1id) + "\t" + data.entityVocab(ep.e2id) + "\t" + data.relVocab(r))
      }
    }
    println("score = " + math.exp(bs.Head.asInstanceOf[AllVariablesHypothesis].score))
    */

    //val result = new EntityPair(ep.e1id, ep.e2id, ep.xCond, rel, z, zScore)
    //val result = new EntityPair(ep.e1id, ep.e2id, ep.xCond, rel, DenseVector(bs.Head.asInstanceOf[AllVariablesHypothesis].z.toArray), zScore, DenseVector(bs.Head.asInstanceOf[AllVariablesHypothesis].obs))
    val result = new EntityPair(ep.e1id, ep.e2id, ep.xCond, rel, DenseVector(bs.Head.asInstanceOf[AllVariablesHypothesis].z.toArray), zScore, bs.Head.asInstanceOf[AllVariablesHypothesis].obs)

    if(Constants.TIMING) {
      Utils.Timer.stop("inferAll")
    }
    result
  }
}
