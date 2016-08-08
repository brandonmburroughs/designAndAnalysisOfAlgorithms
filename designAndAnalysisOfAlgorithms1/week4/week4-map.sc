import scala.collection.mutable.{ListBuffer,HashMap}
import scala.util.{Try, Success, Failure}

case class Graph(nodes: List[Int], edges: List[(Int, Int)])


def loadGraph(fileURL: String): Map[Int, List[Int]] = {
  val graph = HashMap[Int, List[Int]]() withDefaultValue Nil

  val lines = scala.io.Source.fromURL(fileURL).getLines
  lines.foreach{
    line =>
      val lineSplit = line.split(" ").map(_.toInt)
      val (src, dst) = (lineSplit.head, lineSplit.tail.head)
      graph(src) = graph(src) :+ dst
  }
  graph.toMap withDefaultValue Nil
}


def loadReverseGraph(fileURL: String): Map[Int, List[Int]] = {
  val graph = HashMap[Int, List[Int]]() withDefaultValue Nil

  val lines = scala.io.Source.fromURL(fileURL).getLines
  lines.foreach{
    line =>
      val lineSplit = line.split(' ')
      val (dst, src) = (lineSplit.head.toInt, lineSplit.tail.head.toInt)
      graph(src) = graph(src) :+ dst
  }
  graph.toMap withDefaultValue Nil
}


def reverseGraph(graph: Map[Int, List[Int]]): Map[Int, List[Int]] = {
  val reversedGraph = HashMap[Int, List[Int]]() withDefaultValue Nil

  graph.keysIterator.foreach {
    src =>
      val allDst = graph(src)
      allDst.foreach {
        dst =>
          reversedGraph(dst) = reversedGraph(dst) :+ src
      }
  }

  reversedGraph.toMap withDefaultValue Nil
}


def depthFirstSearchLoop(graph: Map[Int, List[Int]], order: List[Int]): (List[Int], List[Int]) = {
  // Initialize various bookkeeping
  val explored = HashMap[Int, Boolean]() withDefaultValue false
  var numberNodesProcessedSoFar = 0
  val finishingTimes = ListBuffer.fill(order.size)(-1)
  var currentLeader = -1
  val leaders = ListBuffer.fill(order.size)(-1)

  def depthFirstSearch(graph: Map[Int, List[Int]], startingPoint: Int): Unit = {
    explored(startingPoint) = true
    leaders(startingPoint - 1) = currentLeader

    val newNodes = graph(startingPoint)

    for (node <- newNodes) {
      if (!explored(node)) {
        depthFirstSearch(graph, node)
      }
    }

    numberNodesProcessedSoFar += 1
    finishingTimes(startingPoint - 1) = numberNodesProcessedSoFar
  }

  // Loop over all nodes in graph, running DFS
  for (i <- order.zipWithIndex.sortBy(_._1).reverse.map(_._2 + 1)) {
    if (!explored(i)) {
      currentLeader = i
      depthFirstSearch(graph, i)
    }
  }

  (finishingTimes.toList, leaders.toList)
}


def stronglyConnectedComponents(graph: Map[Int, List[Int]]): List[List[Int]] = {

  val graphReverse = reverseGraph(graph)
  val (order, _) = depthFirstSearchLoop(graphReverse, (graphReverse.keys.max to 1 by -1).toList)
  val (_, leaders) = depthFirstSearchLoop(graph, order)

  leaders.distinct.map(v => leaders.zipWithIndex.filter(_._1 == v).map(_._2 + 1))
}

def getSccSize(scc: List[List[Int]]): String = {
  if (scc.length < 5) getSccSize(scc :+ List())
  else scc.map(_.length).sorted.reverse.slice(0,5).mkString(",")
}

/**
  * Tests
  */
val testOneInput = "https://raw.githubusercontent.com/brandonmburroughs/designAndAnalysisOfAlgorithms/master/designAndAnalysisOfAlgorithms1/week4/testCases/one.txt"
val testOneOutput = "3,3,3,0,0"

getSccSize(stronglyConnectedComponents(loadGraph(testOneInput))) == testOneOutput

val testTwoInput = "https://raw.githubusercontent.com/brandonmburroughs/designAndAnalysisOfAlgorithms/master/designAndAnalysisOfAlgorithms1/week4/testCases/two.txt"
val testTwoOutput = "3,3,2,0,0"

getSccSize(stronglyConnectedComponents(loadGraph(testTwoInput))) == testTwoOutput

val testThreeInput = "https://raw.githubusercontent.com/brandonmburroughs/designAndAnalysisOfAlgorithms/master/designAndAnalysisOfAlgorithms1/week4/testCases/three.txt"
val testThreeOutput = "3,3,1,1,0"

getSccSize(stronglyConnectedComponents(loadGraph(testThreeInput))) == testThreeOutput

val testFourInput = "https://raw.githubusercontent.com/brandonmburroughs/designAndAnalysisOfAlgorithms/master/designAndAnalysisOfAlgorithms1/week4/testCases/four.txt"
val testFourOutput = "7,1,0,0,0"

getSccSize(stronglyConnectedComponents(loadGraph(testFourInput))) == testFourOutput

val testFiveInput = "https://raw.githubusercontent.com/brandonmburroughs/designAndAnalysisOfAlgorithms/master/designAndAnalysisOfAlgorithms1/week4/testCases/five.txt"
val testFiveOutput = "6,3,2,1,0"

getSccSize(stronglyConnectedComponents(loadGraph(testFiveInput))) == testFiveOutput


/**
  * Homework Questions
  */

val dataFileURL = "https://raw.githubusercontent.com/brandonmburroughs/designAndAnalysisOfAlgorithms/master/designAndAnalysisOfAlgorithms1/week4/scc.txt"

val fiveLargestSCC = getSccSize(stronglyConnectedComponents(loadGraph(dataFileURL)))
println(s"The five largest strongly connected components are $fiveLargestSCC!")