class Mention(features:Array[Int]) { 
}

class EntityPair(e1id:Int, e2id:Int, mentions:Array[Mention]) {
}

class Rel(id:Int, instances:Array[EntityPair]) { 
}

class ProtobufData(inFile:String) { 
  

  val is = new GZIPInputStream(new BufferedInputStream(new FileInputStream(inFile)))
  var r = Relation.parseDelimitedFrom(is);
  while(r != null) {
    println(r.getRelType())

    r.getSourceGuid
    r.getDestGuid

    r = Relation.parseDelimitedFrom(is)
  }
  sValue  
}

