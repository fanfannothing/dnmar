package dnmar;

import scala.collection.mutable.HashMap
import math._
import scala.io._
import java.io.FileWriter
import scala.util.Random

//class Type(entityVocab:Int, eventVocab:Int, alpha:Double) {
class Type(env:Vocab, evv:Vocab, enc:HashMap[Int,Int], evc:HashMap[Int,Int], alpha:Double) {

  def this() = this(new Vocab, new Vocab, new HashMap[Int,Int], new HashMap[Int,Int], 1.0)

  val rnd    = new Random
  val ALPHA  = alpha

  val enVoc = env
  val evVoc = evv

  val enCounts = enc
  val enSum    = enCounts.values.sum
  val evCounts = evc
  val evSum    = evCounts.values.sum

  //TODO: update inference code to update thetaCounts (don't hold as fixed) 

  val thetaEn = new Array[Double](enVoc.nextInt)
  val thetaEv = new Array[Double](evVoc.nextInt)

  def UpdateThetaEn() = {
    for(i <- 0 to enVoc.nextInt-1) {
      thetaEn(i) = (enCounts.getOrElse(i, 0) + ALPHA) / (enSum + ALPHA*enVoc.nextInt)
    }
  }

  def UpdateThetaEv() = {
    for(i <- 0 to evVoc.nextInt-1) {
      thetaEv(i) = (evCounts.getOrElse(i, 0) + ALPHA) / (evSum + ALPHA*evVoc.nextInt)
    }
  }

  UpdateThetaEn()
  UpdateThetaEv()
}

object Utils {
  def deepCopy[A](a: A)(implicit m: reflect.Manifest[A]): A =
    util.Marshal.load[A](util.Marshal.dump(a))

  def count[A](xs:List[A]): HashMap[A,Int] = {
    val result = new HashMap[A,Int]()
    for(x <- xs) {
      result(x) = result.getOrElse(x, 0) + 1
    }
    return result
  }

  object Timer {
    var begin:Long = 0L
    var end:Long = 0L
    def start = {
      begin = System.currentTimeMillis
    }
    def stop(s:String) = {
      end = System.currentTimeMillis
      println(s + ">   " + (end - begin)/ 1000.0 + " s")
    }
  }
}

object FileUtils {
  //Reads in a file containing tuples, filtering out those which fall outside of a specific date range
  def filterTuplesByDate(inFile:String, outFile:String, startDate:Int, endDate:Int) {
    val out = new FileWriter(outFile);
    for(line <- Source.fromFile(inFile).getLines) {
      val Array(sid, creation_seconds, date, eventS, entityS) = StringUtils.stripWS(line).split('\t')
      if(date != "__NONE__" && entityS != "__NONE__" && date.toInt >= startDate && date.toInt <= endDate) {
	out.write(line + "\n");
      }
    }
    out.close
  }

  //TODO: Change this to read from doc-topic and arg1-topic-word files...
  def readTypes(entityFile:String, eventPhraseFile:String, enVoc:Vocab, evVoc:Vocab, alpha:Double):Array[Type] = {
    var result = List()
    var nTypes = 0
    
    //First Pass - Figure out how many types
    for(line <- Source.fromFile(entityFile).getLines) {
      nTypes += 1
    }

    val countMatch = """(.+):(\d+)""".r
    val zEnCounts = new HashMap[Int, HashMap[Int,Int]]
    Source.fromFile(entityFile).getLines.foreach((line) => {
      val fields = StringUtils.stripWS(line).split('\t')
      val z      = fields(0).toInt
      for(i <- 1 to fields.length-1) {
	val countMatch(entity, count) = fields(i)
	if(!zEnCounts.contains(z)) {
	  zEnCounts(z) = new HashMap[Int,Int]
	}
	zEnCounts(z)(enVoc(entity)) = count.toInt
      }
    })

    val zEvCounts = new HashMap[Int, HashMap[Int,Int]]
    Source.fromFile(eventPhraseFile).getLines.foreach((line) => {
      val fields = StringUtils.stripWS(line).split('\t')
      val eventPhrase = evVoc(fields(0))
      for(i <- 1 to fields.length-1) {
	val Array(zStr, count) = fields(i).split(":")
	val Array(z1, z2) = zStr.split("-")
	if(!zEvCounts.contains(z1.toInt)) {
	  zEvCounts(z1.toInt) = new HashMap[Int,Int]
	}
	zEvCounts(z1.toInt)(eventPhrase) = count.toInt
	if(!zEvCounts.contains(z2.toInt)) {
	  zEvCounts(z2.toInt) = new HashMap[Int,Int]
	}
	zEvCounts(z2.toInt)(eventPhrase) = count.toInt
      }
    })
    
    val types = (0 to zEvCounts.keys.size-1).map((z) => {
      new Type(enVoc, evVoc, zEnCounts(z), zEvCounts(z), alpha)
    }).toArray
    
    return(types)
  }
}

object StringUtils {
  def chomp(str:String) : String = {
    str.substring(0, str.lastIndexOf("\n"))
  }

  /** 
   * stripWS
   * Strips leading/trailing whitespace
   */ 
  def stripWS(str:String) : String = {
    str.replaceFirst("""^\s+""", "").replaceFirst("""[\s\n]+$""", "")
  }
}

object MathUtils {
  val rnd = new Random

  def ArgMax(d:Array[Double]):Int = {
    var result = 0
    var max = d(0)
    for(i <- 1 to d.length-1) {
      if(d(i) > max) {
	result = i
	max = d(i)
      }
    }
    return(result)
  }

  //Sample from a discrete distribution
  def Sample(d:Array[Double]):Int = {
    var sum = 0.0
    val target = rnd.nextDouble * d.sum.toDouble
    
    for(i <- 0 to d.length-1) {
      sum += d(i)
      if(sum > target) {
	return(i)
      }
    }
    0
  }

  //NOTE: seems to work...  might be a couple boundary cases.
  def LogNormalize(d:Array[Double]) {
    //Log Exp Sum
    val max = d.max
    val logSum = max + log(d.map(x => exp(x - max)).sum)
    //Normalize
    for(i <- 0 to d.length-1) {
      d(i) -= logSum
    }
  }

  def LogExpSum(d:Array[Double]):Double = {
    val max = d.max
    return(max + log(d.map(x => exp(x - max)).sum))
  }

  def Normalize(d:Array[Double]):Array[Double] = {
    val sum = d.sum
    for(i <- 0 to d.length-1) {
      d(i) /= sum
    }
    d
  }

  def LogFactorial(n:Double):Double = {
    var result = 0.0
    for(i <- 2 to n.toInt) {
      result += log(i)
    }
    return result
  }

  def LogFactorial_old(n:Double):Double = {
    if(n <= 1) {
      return 0.0;
    } else {
      return log(n) + LogFactorial(n-1);
    }
  }

  //Rising factorial function
  def LogRff(n:Double, alpha:Double):Double = {
    if(n <= 1) {
      return 0.0;
    } else {
      return log(alpha + n) + LogFactorial(n-1);
    }    
  }
}
