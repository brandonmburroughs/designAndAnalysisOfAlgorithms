// This is not my code, but an idea (with code) I found on Github and altered.
// See here:  https://github.com/davidalk/stanford-algorithms-pt1/blob/8c5272d35c3efc610d85de6b9cfa58d1ece1d948/src/main/scala/week4/StronglyConnectedComponents.scala
import scala.collection.mutable.HashMap
import scala.io.Source
import scala.collection.mutable.Stack

object Util {
  def loadDirectedGraph(filename: String): Map[Int, List[Int]] = {
    val rows = Source.fromFile(filename).getLines
    val returnMap = HashMap[Int, List[Int]]()
    rows foreach { r =>
      val rowList = r.split(" ").toList.map(_.toInt)
      val tailVertex = rowList(0)
      val headVertex = rowList(1)
      val edges = returnMap.getOrElse(tailVertex, Nil)
      returnMap.put(tailVertex, headVertex :: edges)
    }
    returnMap.toMap
  }

  def loadInvertedDirectedGraph(filename: String): Map[Int, List[Int]] = {
    val rows = Source.fromFile(filename).getLines
    val returnMap = HashMap[Int, List[Int]]()
    rows foreach { r =>
      val rowList = r.split(" ").toList.map(_.toInt)
      val tailVertex = rowList(1)
      val headVertex = rowList(0)
      val edges = returnMap.getOrElse(tailVertex, Nil)
      returnMap.put(tailVertex, headVertex :: edges)
    }
    returnMap.toMap

  }
}


object StronglyConnectedComponents {

  def main(args: Array[String]): Unit = {
    val graph = Util.loadDirectedGraph("/media/bburroughs/Seagate_2TB/Coursera/designAndAnalysisOfAlgorithms/designAndAnalysisOfAlgorithms1/week4/scc.txt")

    val order = dfs(graph)
    //println(order)
    val invertedGraph = Util.loadInvertedDirectedGraph("/media/bburroughs/Seagate_2TB/Coursera/designAndAnalysisOfAlgorithms/designAndAnalysisOfAlgorithms1/week4/scc.txt")
    val scc = dfsSccCount(invertedGraph, order)
    println(scc.sorted.reverse)

  }

  def dfsSccCount(graph: Map[Int, List[Int]], order: List[Int]): List[Int] = {
    //println("starting scc count")

    val defaultGraph = graph.withDefault(_ => List())

    val visited = scala.collection.mutable.HashMap[Int, Boolean]().withDefault(_ => false)

    var sccList = List[Int]()

    def dfsVisit(u: Int): Int = {
      val stack = Stack[Int]()
      stack.push(u)
      var count = 0

      while (stack.nonEmpty) {
        val v = stack.pop
        if (!visited(v)) {
          count = count + 1
          visited(v) = true
          defaultGraph(v) map { stack.push }
        }
      }
      count
    }

    for (vertex <- order) {
      if (!visited(vertex)) {
        //println("Start visit at " + vertex)
        sccList = dfsVisit(vertex) :: sccList
      }
    }

    sccList
  }

  def dfs(graph: Map[Int, List[Int]]): List[Int] = {
    var order = List[Int]()
    val defaultGraph = graph.withDefault(_ => List())
    val visited = scala.collection.mutable.HashMap[Int, Boolean]().withDefault(_ => false)

    def dfsVisit(u: Int): Unit = {
      val stack = Stack[Int]()
      stack.push(u)

      while (stack.nonEmpty) {
        val v = stack.pop
        if (!visited(v)) {
          visited(v) = true
          order = v :: order
          defaultGraph(v) map { stack.push }
        }
      }
    }

    for (vertex <- defaultGraph.keys) {
      if (!visited(vertex)) {
        //println("Start visit at " + vertex)
        dfsVisit(vertex)
      }
    }

    order

  }
}

StronglyConnectedComponents.main(Array[String]())