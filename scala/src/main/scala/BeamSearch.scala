package dnmar;

import scala.collection.mutable.PriorityQueue

//A hypothesis has a score, in addition to an array of sucessors
abstract class Hypothesis extends Ordered[Hypothesis] {
  def score: Double
  def sucessors: Array[Hypothesis]

  def priority = score

  def compare(that:Hypothesis):Int = {
    if(this.score > that.score) {
      1
    } else if(this.score < that.score) {
      -1
    } else {
      0
    }
  }
}

class BeamSearch(h:Hypothesis, beamSize:Int) {
  def this(h:Hypothesis) = this(h, 3)

  val _BEAM_SIZE=beamSize
  var hypothesisQueue = new PriorityQueue[Hypothesis]
  this.hypothesisQueue += h

  def Head:Hypothesis = hypothesisQueue.head

  def UpdateQ() {
    Utils.Timer.start
    val next = hypothesisQueue.dequeue
    hypothesisQueue ++= next.sucessors
    
    if(hypothesisQueue.size > _BEAM_SIZE) {
      val newQ = new PriorityQueue[Hypothesis]
      while(!hypothesisQueue.isEmpty && newQ.size < _BEAM_SIZE) {
	newQ += hypothesisQueue.dequeue()
      }
      hypothesisQueue = newQ
    }
    Utils.Timer.stop("UpdateQ")
  }
}
