case class Graph(name:String, neighbors:Array[Graph])

class GraphHypothesis(g:Graph, s:Double) extends Hypothesis {
  val graph = g
  val score = s
  val sucessors:Array[Hypothesis] = g.neighbors.map((x) => new GraphHypothesis(x, score + 1)).toArray
}

object GraphTest {
  def main(args: Array[String]) {
    val graph = Graph("A", Array(Graph("B", Array(Graph("E", Array()), Graph("F", Array(Graph("G", Array()))))), Graph("C", Array(Graph("D", Array())))))
    val gh = new GraphHypothesis(graph, 0)
    val bs = new BeamSearch(gh)

    for(i <- 1 to 10) {
      println(bs.q.map((x) => x.asInstanceOf[GraphHypothesis].graph.name + "\t" + x.score))
      bs.UpdateQ
    }
  }
}
