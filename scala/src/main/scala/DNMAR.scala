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

import math._

class HiddenVariablesHypothesisTwoSided(postZ:DenseMatrix[Double], postObs:DenseVector[Double], zPart:List[Int], rPart:Array[Double], obs:Array[Double], sPartial:Double, val score:Double) extends Hypothesis {
  def z:Array[Int] = {
    return zPartial.reverse.toArray
  }

  var rPartial = rPart

  var zPartial = zPart 

  def sucessors:Array[Hypothesis] = {
    val result = new ListBuffer[Hypothesis]

    if(zPartial.length == postZ.numRows) {
      return result.toArray
    }

    for(rel <- 0 until postZ.numCols) {
      val newZ = rel :: zPartial
      var newSpartial = sPartial + postZ(z.length, rel)
      var newScore = newSpartial

      //Update rPartial
      val newRpartial = rPartial.clone
      newRpartial(rel) = 1.0

      //Add max scores for rest of z's (note: this is an upper bound / admissible heuristic)
      for(i <- newZ.length until postZ.numRows) {
	newScore += postZ(i,::).max
      }

      //Observation factors
      for(rel <- 0 until postZ.numCols) {
	if(newRpartial(rel) == 1.0) {
	  newScore += postObs(rel)
	} else if(postObs(rel) > 0.0) {
	  //Find the bess possible way of changing one of the remaining z's (note: it is possible we could use the same z to satisfy 2 different relations, but hey this is an upper bound!)
	  var maxValue = Double.NegativeInfinity
	  for(i <- newZ.length until postZ.numRows) {
	    val possibleValue = postObs(rel) + postZ(i,rel) - postZ(i,::).max
	    if(possibleValue > maxValue) {
	      maxValue = possibleValue
	    }
	  }
	  if(maxValue > 0.0) {
	    newScore += maxValue
	  }
	}
      }

      result += new HiddenVariablesHypothesisTwoSided(postZ, postObs, newZ, newRpartial, obs, newSpartial, newScore)
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
  //val training = Random.shuffle((0 until data.data.length).toList).filter((e12) => data.data(e12).rel(data.relVocab("NA")) == 0.0 || scala.util.Random.nextDouble < 0.5)
  val training = Random.shuffle((0 until data.data.length).toList).filter((e12) => true)

  //TODO: seperate training for theta & phi?

  var trainSimple = false

  var updatePhi   = true
  var updateTheta = true

  def train(nIter:Int) = { 
    for(i <- 0 until nIter) {
      //println("iteration " + i)
      var j = 0
      for(e12 <- training) {
	//print("entity pair " + j + "/" + training.length + ":" + data.data(e12).features.length)
	//Run le inference
	val iAll    = inferAll(data.data(e12))
	var iHidden:EntityPair = null  //Just needed to asign it something temporarily...
	var score = 0.0

	if(trainSimple) {
	  iHidden = inferHiddenSimple(data.data(e12))
	} else {
	  //val (iHidden, score) = inferHiddenLocalSearch(data.data(e12), 10)
	  val result = inferHiddenLocalSearch(data.data(e12), 1)
	  iHidden = result._1
	  score   = result._2

	  //Figure out search error (in cases where we can efficiently do exact inference)
	  if(Constants.DEBUG) {
	    if(data.data(e12).features.length < 200) {
	      val (iHidden1rs,  score1rs) = inferHiddenLocalSearch(data.data(e12), 1)
	      val (iHidden10rs, score10rs) = inferHiddenLocalSearch(data.data(e12), 1)
	      val (iHidden20rs, score20rs) = inferHiddenLocalSearch(data.data(e12), 20)
	      val (iHiddenExact, scoreExact) = inferHiddenTwoSided(data.data(e12))
	      println("scores:" + "\t" + score1rs + "\t" + score10rs + "\t" + score20rs + "\t" + scoreExact + "\t" + data.data(e12).features.length)
	    }
	  }
	}

	if(updateTheta) {
	  updateTheta(iAll, iHidden)
	}
	if(updatePhi) {
	  updatePhi(iAll, iHidden)
	}
	j += 1
      }
    }
  }

  def inferHiddenLocalSearch(ep:EntityPair, nRandomRestarts:Int):Tuple2[EntityPair,Double] = {
    if(Constants.TIMING) {
      Utils.Timer.start("inferHiddenLocalSearch")
    }

    //println("N:" + ep.features.length)

    val postZ  = DenseMatrix.zeros[Double](ep.features.length, data.nRel)
    for(i <- 0 until ep.features.length) {
      postZ(i,::) := (theta * ep.features(i)).toDense
    }

    //Posterior distribution over observations
    val postObs     = DenseVector.zeros[Double](data.nRel)
    for(r <- 0 until data.nRel) {
      if(r == data.relVocab("NA")) {
	//postObs(r) = -4.0
	//postObs(r) = -2.0
	postObs(r) = 0.0
      } else if(ep.obs(r) == 0.0) {
	var s = 0.0
	s += phiNeg(data.entityVocab.size + r)
	s += phiNeg(phiNeg.length-1)
	postObs(r) = s
	//postObs(r) = -5.0
      } else {
	var s = 0.0
	s += phiPos(data.entityVocab.size + r)
	s += phiPos(phiPos.length-1)
	postObs(r) = s
	//postObs(r) = 100.0
      }
    }

    var bestZ:DenseVector[Int]      = null
    var bestRel:DenseVectorRow[Double] = null
    var bestScore                   = Double.NegativeInfinity

    for(n <- 0 until nRandomRestarts) {
      val z       = DenseVector.zeros[Int](postZ.numRows)
      val rel     = DenseVector.zeros[Double](postZ.numCols).t
      val rCounts = DenseVector.zeros[Int](postZ.numCols)
      var score = 0.0
      
      //Random initialization
      for(i <- 0 until z.length) {
	//z(i) = scala.util.Random.nextInt(postObs.length)
	z(i) = MathUtils.Sample(scalala.library.Library.exp(postZ(i,::)).toArray)
	score += postZ(i,z(i))
	rCounts(z(i)) += 1
	rel(z(i)) = 1.0
      }
      for(r <- 0 until rCounts.length) {
	if(rCounts(r) > 0) {
	  score += postObs(r)
	}
      }

      var changed = false
      do {
	//Recompute Deltas
	if(Constants.TIMING) {
	  Utils.Timer.start("re-computing deltas")
	}

	//First search operator (change one variable)
	val deltas = DenseMatrix.zeros[Double](postZ.numRows, postZ.numCols)
	for(i <- (0 until postZ.numRows)) {
	  for(r <- 0 until postZ.numCols) {
	    if(r != z(i)) {
	      deltas(i,r) = postZ(i,r) - postZ(i,z(i))
	      if(rCounts(r) == 0) {
		//This will be the first instance of r to be extracted...
		deltas(i,r) += postObs(r)
	      }
	      if(rCounts(z(i)) == 1) {
		//z(i) is the last instance of r remaining...
		deltas(i,r) -= postObs(z(i))
	      }
	    } else {
	      deltas(i,r) = 0.0
	    }
	  }
	}

	//Second search operator (switch all instances of relation r to NA)
	//val deltasNA = DenseVector.zeros[Double](postZ.numCols)
	val deltasAggregate = DenseMatrix.zeros[Double](postZ.numCols, postZ.numCols)
	for(r1 <- 0 until postZ.numCols) {
	  for(r2 <- 0 until postZ.numCols) {
	    if(rCounts(r1) > 0 && r1 != r2) {
	      for(i <- 0 until postZ.numRows) {
		if(z(i) == r1) {
		  deltasAggregate(r1,r2) += postZ(i,r2) - postZ(i,r1)
		}
	      }
	      deltasAggregate(r1,r2) -= postObs(r1)
	      if(rCounts(r2) == 0) {
		deltasAggregate(r1,r2) += postObs(r2)
	      }
	    }
	  }
	}

	if(Constants.TIMING) {
	  Utils.Timer.stop("re-computing deltas")
	}

	changed = false

	val (i, newRel)    = deltas.argmax
	val oldRel         = z(i)
	val delta          = deltas(i,newRel)
	val deltaAggregate = deltasAggregate.max

	//Check which search operator provides the greatest score delta
	if(deltaAggregate > delta && deltaAggregate > 0) {
	  //Change all instances of the max deltaNA relation to NA 
	  score += deltaAggregate

	  val (r1, r2)  = deltasAggregate.argmax
	  for(i <- 0 until z.length) {
	    if(z(i) == r1) {
	      z(i) = r2
	      rCounts(r2) += 1
	    }
	  }
	  rCounts(r1) = 0
	  rel(r1) = 0.0
	  rel(r2) = 1.0
	  changed = true
	} else if(oldRel != newRel && delta > 0) {
	  score += delta
	  z(i) = newRel
	  rCounts(newRel) += 1
	  rel(newRel) = 1.0
	  rCounts(oldRel) -= 1
	  if(rCounts(oldRel) == 0.0) {
	    rel(oldRel) = 0
	  }
	  changed = true
	}
      } while(changed)

      if(score > bestScore) {
	bestScore = score
	bestZ     = z
	bestRel   = rel
      }
    }


    if(Constants.DEBUG) {
      println("constrained score=\t"    + bestScore)
      println("constrained result.z=\t" + bestZ.toList.map((r) => data.relVocab(r)))
      println("constrained rel=\t"      + (0 until bestRel.length).filter((r) => bestRel(r) == 1.0).map((r) => data.relVocab(r)))
      println("constrained obs=\t"      + (0 until ep.obs.length).filter((r) => ep.obs(r) == 1.0).map((r) => data.relVocab(r)))
    }

    val result = new EntityPair(ep.e1id, ep.e2id, ep.features, bestRel, bestZ, null)
    
    if(Constants.TIMING) {
      Utils.Timer.stop("inferHiddenLocalSearch")
    }

    (result, bestScore)
  }

  def inferHiddenTwoSided(ep:EntityPair):Tuple2[EntityPair,Double] = {
    if(Constants.TIMING) {
      Utils.Timer.start("inferHiddenTwoSided")
    }

    val postZ  = DenseMatrix.zeros[Double](ep.features.length, data.nRel)

    for(i <- 0 until ep.features.length) {
      postZ(i,::) := (theta * ep.features(i)).toDense
    }

    //Posterior distribution over observations
    val postObs     = DenseVector.zeros[Double](data.nRel)
    for(r <- 0 until data.nRel) {
      if(r == data.relVocab("NA")) {
	//postObs(r) = -2.0
	postObs(r) = -4.0
      } else if(ep.obs(r) == 0.0) {
	postObs(r) = -5.0
      } else {
	postObs(r) = 100.0
      }
    }

    val rPartial = new Array[Double](data.nRel)
    var sPartial = 0.0
    //val bs = new BeamSearch(new HiddenVariablesHypothesisTwoSided(postZ, postObs, Nil, rPartial, ep.obs.toArray, sPartial, sPartial), 1000);
    val bs = new BeamSearch(new HiddenVariablesHypothesisTwoSided(postZ, postObs, Nil, rPartial, ep.obs.toArray, sPartial, sPartial), -1);

    while(bs.Head.asInstanceOf[HiddenVariablesHypothesisTwoSided].z.length < ep.features.length) {
      bs.UpdateQ
    }

    val rel    = DenseVector.zeros[Double](data.nRel).t
    for(r <- bs.Head.asInstanceOf[HiddenVariablesHypothesisTwoSided].z.toArray) {
      rel(r) = 1.0
    }

    val score     = bs.Head.asInstanceOf[HiddenVariablesHypothesisTwoSided].score

    if(Constants.DEBUG) {
      val z         = bs.Head.asInstanceOf[HiddenVariablesHypothesisTwoSided].z
      val rPartial  = bs.Head.asInstanceOf[HiddenVariablesHypothesisTwoSided].rPartial
      println("constrained score=\t"  + score)
      println("constrained result.z=\t" + z.toList.map((r) => data.relVocab(r)))
      println("constrained rel=\t"      + (0 until rel.length).filter((r) => rel(r) == 1.0).map((r) => data.relVocab(r)))
      println("constrained rPartial=\t" + (0 until rel.length).filter((r) => rPartial(r) == 1.0).map((r) => data.relVocab(r)))
      println("constrained obs=\t" + (0 until rel.length).filter((r) => ep.obs(r) == 1.0).map((r) => data.relVocab(r)))
    }

    val result = new EntityPair(ep.e1id, ep.e2id, ep.features, rel, DenseVector(bs.Head.asInstanceOf[HiddenVariablesHypothesisTwoSided].z.toArray), null, ep.obs)

    if(Constants.TIMING) {
      Utils.Timer.stop("inferHiddenTwoSided")
    }

    (result, score)
  }

  def inferHiddenSimple(ep:EntityPair):EntityPair = {
    if(Constants.TIMING) {
      Utils.Timer.start("inferHidden")
    }

    val z      = DenseVector.zeros[Int](ep.features.length)
    val zScore = DenseVector.zeros[Double](ep.features.length)
    val postZ  = DenseMatrix.zeros[Double](ep.features.length, data.nRel)

    for(i <- 0 until ep.features.length) {
      postZ(i,::) := (theta * ep.features(i)).toDense

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

    val result = new EntityPair(ep.e1id, ep.e2id, ep.features, ep.rel, z, zScore)

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
    val z      = DenseVector.zeros[Int](ep.features.length)
    //val postZ  = new Array[SparseVector[Double]](ep.features.length)
    val postZ  = DenseMatrix.zeros[Double](ep.features.length, data.nRel)
    val zScore = DenseVector.zeros[Double](ep.features.length)
    val rel    = DenseVector.zeros[Double](data.nRel).t

    for(i <- 0 until ep.features.length) {
      if(useAverage) {
	postZ(i,::) := (theta_average * ep.features(i)).toDense
      } else {
	postZ(i,::) := (theta * ep.features(i)).toDense
      }

      z(i) = postZ(i,::).argmax
      zScore(i) = postZ(i,::).max

      //Set the aggregate variables
      rel(z(i)) = 1.0
    }

    if(Constants.DEBUG) {
      val rels  = rel
      println("unconstrained result.z=" + z.toList.map((r) => data.relVocab(r)))
      println("unconstrained rel=" + (0 until rels.length).filter((r) => rels(r) == 1.0).map((r) => data.relVocab(r)))
    }

    val postObs = DenseVector.zeros[Double](data.nRel)
    val newObs  = DenseVector.zeros[Double](data.nRel)

    //TODO: not quite right here...
    for(r <- 0 until data.nRel) {
      if(rel(r) == 1.0) {
	var s = 0.0
	//s += phiPos(ep.e1id)
	//s += phiPos(ep.e2id)
	s += phiPos(data.entityVocab.size + r)
	s += phiPos(phiPos.length-1)	//Bias feature
	postObs(r) = s
      } else {
	//s += phiNeg(ep.e1id)
	//s += phiNeg(ep.e2id)
	s += phiNeg(data.entityVocab.size + r)
	s += phiNeg(phiNeg.length-1)	//Bias feature
	postObs(r) = s        
      }

      //if(rel(r) == 1.0 && postObs(r) > 0.0) {
      if(postObs(r) > 0) {
	newObs(r) = 1.0
      }
    }

    //println("unconstrained obs=" + newObs.toList)
    //println("unconstrained postObs=" + postObs.toList)

    //TODO: get rid of zScore, replace with postZ...
    val result = new EntityPair(ep.e1id, ep.e2id, ep.features, rel, z, zScore, newObs)
    result.postObs = postObs

    if(Constants.TIMING) {
      Utils.Timer.stop("inferAll")
    }
    result
  }
}
