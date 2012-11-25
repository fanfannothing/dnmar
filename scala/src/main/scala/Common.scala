package dnmar;

import scala.collection.mutable.HashMap
import math._
import scala.io._
import java.io._
import java.io.FileWriter
import scala.util.Random

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

  def bin2int(b:Array[Double]):List[Int] = {
    var result = List[Int]()
    for(i <- 0 until b.length) {
      if(b(i) == 1.0) {
	result ::= i
      }
    }
    return result.reverse
  }

  object Timer {
    var begin = new HashMap[String,Long]
    var sum   = new HashMap[String,Long]

    def reset {
      begin = new HashMap[String,Long]
      sum   = new HashMap[String,Long]
    }

    def reset(s:String):Double = {
      val time = stop(s)
      begin.remove(s)
      sum.remove(s)
      time
    }

    def start(s:String) = {
      if(Constants.DEBUG) {
	println("start " + s)
      }
      begin(s) = System.currentTimeMillis
    }

    def stop(s:String):Double = {
      if(Constants.DEBUG) {
	println("stop " + s)
      }
      val end = System.currentTimeMillis
      sum(s) = sum.getOrElse(s, 0L) + (end - begin(s))
      sum(s) / 1000.0
    }

    def print {
      for((s,t) <- sum.toList.sortBy(_._2).reverse) {
	println(s + "\t" + t / 1000.0 + " s")
      }
    }
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

object FreebaseUtils {
  def guid2mid(GUID:String) : String = {
    //From Freebase wiki:
    /*
     * 1. Take the GUID
     * 2. Strip off the leading "#9202a8c04000641f8"
     * 3. Take what's left and interpret it as a hex number
     * 4. Express that number in base 32 using the character set 0123456789bcdfghjklmnpqrstvwxyz_ (ie the digits, the letters excluding the vowels and underscore)
     * Prepend /m/0 to what you've got.
     */
    val characters =List('0','1','2','3','4','5','6','7','8','9','b','c','d','f','g','h','j','k','l','m','n','p','q','r','s','t','v','w','x','y','z','_').toArray

    var result = List[Char]()
    var number = Integer.parseInt(GUID.slice(17,GUID.length), 16)
    println(GUID)
    println(GUID.slice(17,GUID.length))
    while(number > 0) {
      result ::= characters(number & 31)
      number >>= 5
    }
    
    return "/m/0" + result.mkString("")
  }

  /************************************************************************************
   * Filter freebase quadruple dump to generate a file only containing relevant
   * entities (acording to the GUID vocab).  This is basically just for efficiency...
   ************************************************************************************
   */
  def filterFreebase(sourceFile:String, targetFile:String, guidVocab:String) {
//InputStream fileStream = new FileInputStream(filename);
//InputStream gzipStream = new GZIPInputStream(fileStream);
//Reader decoder = new InputStreamReader(gzipStream, encoding);
//BufferedReader buffered = new BufferedReader(decoder);

    val buffered = new BufferedInputStream(new InputStreamReader(new org.apache.tools.bzip2.CBZip2InputStream(new FileInputStream(sourceFile))))
    
    val locked = guidVocab.locked
    guidVocab.locked = true
    for(line <- buffered.getlines()) {
      var Array(a1, a2, a3, a4) = line.strip.split("\t")
      if(guidVocab(a1) > 0 || guidVocab(a3) || guidVocab(a4) > 0) {
	println(line)
      }
    }
    guidVocab.locked = locked
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

  //Not the most efficient...
  def Mode[T](d:Array[T]) {
    var maxCount = 0
    var maxVal = d(0)

    for(i <- 0 until d.length) {
      var count = 0
      for(j <- (i+1) until d.length) {
	if(d(j) == d(i)) {
	  count += 1
	}
      }
      if(count > maxCount) {
	maxCount = count
	maxVal = d(i)
      }
    }
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
