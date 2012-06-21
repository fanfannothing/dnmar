import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet

// Vocabulary class maps strings to integers for efficiency
class Vocab {
  var string2Int = new HashMap[String, Int]
  var int2String = new HashMap[Int, String]
  val unk = 0
  var nextInt = 1;
  var locked = false
  
  def apply(str:String):Int = {
    if(!string2Int.contains(str) && locked) {
      return 0  //UNK is 0
    }
    else if(!string2Int.contains(str)) {
      string2Int += str -> nextInt
      int2String += nextInt -> str
      nextInt += 1
    }
    return string2Int(str)
  }

  def apply(i:Int):String = {
    if(!int2String.contains(i)) {
      "-UNK-"
    } else {
      int2String(i)
    }
  }
}
