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

class HiddenVariablesHypothesis(postZ:DenseMatrix[Double], postObs:DenseVector[Double], zPartial:ListBuffer[Int], var rPartial:Array[Double], val obs:Array[Double], var sPartial:Double, var score:Double) extends Hypothesis {
  val z = zPartial

  //println("score=" + score)

  def sucessors:Array[Hypothesis] = {
    val result = new ListBuffer[Hypothesis]

    for(rel <- 0 until postZ.numCols) {

      val newZ = z.clone() += rel
      //var newSpartial = sPartial + math.log(postZ(z.length, rel))
      var newSpartial = sPartial + postZ(z.length, rel)
      var newScore = newSpartial

      //println("sPartial=" + sPartial + "\tnewSpartial =" + newSpartial)

      //Update rPartial
      val newRpartial = rPartial.clone
      newRpartial(rel) = 1.0

      //println("newScore1=" + newScore)

      //Observation factors
      for(rel <- 0 until postZ.numCols) {
        if(newRpartial(rel) == 1.0) {
	  //TODO: Not so sure here...
	  if(obs(rel) > 0.5) {
            //newScore += math.log(postObs(rel))
	    newScore += postObs(rel)
	  }
//	  } else {
//	    newScore += math.log(1.0 - postObs(rel))
//	  }
        }
      }

      //Need to extract all true facts...?
      if(obs(rel) == 1.0 && newRpartial(rel) == 0.0) {
	newScore = Double.NegativeInfinity
      }

      //Add max scores for rest of z's (note: this is an upper bound / admissible heuristic)
      for(i <- newZ.length until postZ.numRows) {
        //newScore += math.log(postZ(i,::).max)
	newScore += postZ(i,::).max
      }

      //println("newScore3=" + newScore)

      if(newScore > Double.NegativeInfinity) {
	result += new HiddenVariablesHypothesis(postZ, postObs, newZ, rPartial, obs, newSpartial, newScore)
      }
    }

    return result.toArray
  }
}

class DNMAR(data:EntityPairData) extends Parameters(data) {
  //TODO: without the assumption that unobserved data are negatives, shouldn't need to throw out 80% of negative data...

  //Randomly permute the training data
  //Throw out X% of negative data...
  //val training = Random.shuffle((0 until data.data.length).toList).filter((e12) => data.data(e12).rel(data.relVocab("NA")) == 0.0 || scala.util.Random.nextDouble < 0.1)
  //val training = Random.shuffle((0 until data.data.length).toList).filter((e12) => data.data(e12).rel(data.relVocab("NA")) == 0.0 || scala.util.Random.nextDouble < 0.2)
  val training = Random.shuffle((0 until data.data.length).toList).filter((e12) => data.data(e12).rel(data.relVocab("NA")) == 0.0 || scala.util.Random.nextDouble < 0.5)
  //val training = Random.shuffle((0 until data.data.length).toList).filter((e12) => true)

  var trainSimple = false

  var updatePhi   = true
  var updateTheta = true

  def train(nIter:Int) = { 
    for(i <- 0 until nIter) {
      //println("iteration " + i)
      for(e12 <- training) {
	//Run le inference
	val iAll    = inferAll(data.data(e12))
	var iHidden = iAll  //Just needed to asign it something temporarily...

	if(trainSimple) {
	  iHidden = inferHiddenSimple(data.data(e12))
	} else {
	  iHidden = inferHidden(data.data(e12))
	}

	if(updateTheta) {
	  updateTheta(iAll, iHidden)
	}
	if(updatePhi) {
	  updatePhi(iAll, iHidden)
	}
      }
    }
  }

  def inferHidden(ep:EntityPair):EntityPair = {
    if(Constants.TIMING) {
      Utils.Timer.start("inferHidden")
    }

    val postZ  = DenseMatrix.zeros[Double](ep.xCond.length, data.nRel)
    //TODO: zScore doesn't really make sense when we are using beam search, but this is how evaluation is done...
    val rel    = DenseVector.zeros[Double](data.nRel).t

    for(i <- 0 until ep.xCond.length) {
      postZ(i,::) := (theta * ep.xCond(i)).toDense
      //TODO: should we really be doing this?  locally normalized?  not so sure...
      //postZ(i,::) -= MathUtils.LogExpSum(postZ(i,::).toArray)
    }

    //Posterior distribution over observations
    val postObs = DenseVector.zeros[Double](data.nRel)
    for(r <- 0 until data.nRel) {
      var s = 0.0
      //s += phi(ep.e1id)
      //s += phi(ep.e2id)
      s += phi(data.entityVocab.size + r)
      s += phi(phi.length-1)	//Bias feature
      //TODO: should we really be doing this?  locally normalized?  not so sure...
      //val p = 1.0 / (1.0 + exp(-s))
      //postObs(r) = p
      postObs(r) = s
      //postObs(r) = 0.9
      //postObs(r) = 1.0
    }

    //println("constrained postObs=" + postObs.toList)

    val bs = new BeamSearch(new HiddenVariablesHypothesis(postZ, postObs, new ListBuffer[Int], Array.fill[Double](data.nRel)(0.0), ep.obs.toArray, 0.0, 0.0), 10);
    
    while(bs.Head.asInstanceOf[HiddenVariablesHypothesis].z.length < ep.xCond.length) {
      //println(bs.Head.asInstanceOf[HiddenVariablesHypothesis].z.length)
      bs.UpdateQ
    }

    if(Constants.DEBUG) {
      val z     = bs.Head.asInstanceOf[HiddenVariablesHypothesis].z
      val score = math.exp(bs.Head.asInstanceOf[HiddenVariablesHypothesis].score)
      println("constrained score = " + score)
      println("constrained result.z=" + z.toList.map((r) => data.relVocab(r)))
    }

    //TODO: a bit of reorganizing/refactoring here maybe...
    for(r <- bs.Head.asInstanceOf[HiddenVariablesHypothesis].z.toArray) {
      rel(r) = 1.0
    }

    /* Some Debugging code...
     * /
    for(r <- 0 until ep.rel.length) {
      if(rel(r) == 1.0) {
        println("rel:" + rel(r) + "\t" + data.entityVocab(ep.e1id) + "\t" + data.entityVocab(ep.e2id) + "\t" + data.relVocab(r))
      }
    }

    for(r <- 0 until ep.rel.length) {
      if(ep.obs(r) == 1.0) {
        println("obs:" + ep.obs(r) + "\t" + data.entityVocab(ep.e1id) + "\t" + data.entityVocab(ep.e2id) + "\t" + data.relVocab(r))
      }
    }
    */

    //val result = new EntityPair(ep.e1id, ep.e2id, ep.xCond, rel, DenseVector(bs.Head.asInstanceOf[HiddenVariablesHypothesis].z.toArray), zScore, DenseVector(bs.Head.asInstanceOf[HiddenVariablesHypothesis].obs))
    val result = new EntityPair(ep.e1id, ep.e2id, ep.xCond, rel, DenseVector(bs.Head.asInstanceOf[HiddenVariablesHypothesis].z.toArray), null, ep.obs)

    if(Constants.TIMING) {
      Utils.Timer.stop("inferHidden")
    }
    result
  }

  def inferHiddenSimple(ep:EntityPair):EntityPair = {
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
      //postZ(i,::) := exp((theta * ep.xCond(i)).toDense)
      postZ(i,::) := (theta * ep.xCond(i)).toDense

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

  /*
   * Greedy search for best overall assignment to z, aggregate rel and obs
   * (1) find best assignment to z
   * (2) compute rel (deterministically)
   * (3) predict max observation value for each fact 
   */
  def inferAll(ep:EntityPair):EntityPair = {
    inferAll(ep, false)
  }

  def inferAll(ep:EntityPair, useAverage:Boolean):EntityPair = {
    if(Constants.TIMING) {
      Utils.Timer.start("inferAll")
    }
    val z      = DenseVector.zeros[Int](ep.xCond.length)
    //val postZ  = new Array[SparseVector[Double]](ep.xCond.length)
    val postZ  = DenseMatrix.zeros[Double](ep.xCond.length, data.nRel)
    val zScore = DenseVector.zeros[Double](ep.xCond.length)
    val rel    = DenseVector.zeros[Double](data.nRel).t

    for(i <- 0 until ep.xCond.length) {
      if(useAverage) {
	//postZ(i) = theta_average * ep.xCond(i)	
	postZ(i,::) := (theta_average * ep.xCond(i)).toDense
      } else {
	//postZ(i) = theta * ep.xCond(i)
	postZ(i,::) := (theta * ep.xCond(i)).toDense
      }

      //z(i) = postZ(i).argmax
      z(i) = postZ(i,::).argmax
      //zScore(i) = postZ(i).max
      zScore(i) = postZ(i,::).max

      //Set the aggregate variables
      rel(z(i)) = 1.0
    }
    if(Constants.DEBUG) {
      println("unconstrained result.z=" + z.toList.map((r) => data.relVocab(r)))
    }

    val postObs = DenseVector.zeros[Double](data.nRel)
    val newObs  = DenseVector.zeros[Double](data.nRel)
    for(r <- 0 until data.nRel) {
      var s = 0.0
      //s += phi(ep.e1id)
      //s += phi(ep.e2id)
      s += phi(data.entityVocab.size + r)
      s += phi(phi.length-1)	//Bias feature
      //val p = 1.0 / (1.0 + exp(-s))
      //postObs(r) = p
      postObs(r) = s

      //if(rel(r) == 1.0 && postObs(r) > 0.5) {
      if(rel(r) == 1.0 && postObs(r) > 0.0) {
	newObs(r) = 1.0
      }
    }

    //println("unconstrained obs=" + newObs.toList)
    //println("unconstrained postObs=" + postObs.toList)

    //TODO: get rid of zScore, replace with postZ...
    val result = new EntityPair(ep.e1id, ep.e2id, ep.xCond, rel, z, zScore, newObs)
    result.postObs = postObs

    if(Constants.TIMING) {
      Utils.Timer.stop("inferAll")
    }
    result
  }
}
