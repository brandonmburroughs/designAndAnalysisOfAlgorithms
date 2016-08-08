import scala.collection.mutable.ListBuffer

case class Graph(nodes: List[Int], edges: List[(Int, Int)])

def loadGraph(stringList: List[String]): Graph = {
  val nodes = stringList.flatMap(line => line.split(' ').map(_.toInt)).distinct
  val edges = stringList.map{
    line =>
      val splitLine = line.split(' ')
      val (src, dst) = (splitLine.head.toInt, splitLine.tail.head.toInt)
      (src, dst)
  }

  Graph(nodes, edges)
}


def reverseGraph(graph: Graph): Graph = {
  val newEdges = graph.edges.map{
    case (src, dst) => (dst, src)
  }

  Graph(graph.nodes, newEdges)
}


def depthFirstSearchLoop(graph: Graph, order: List[Int]): (List[Int], List[Int]) = {
  // Initialize various bookkeeping
  val explored = ListBuffer.fill(graph.nodes.size)(false)
  var numberNodesProcessedSoFar = 0
  val finishingTimes = ListBuffer.fill(graph.nodes.size)(-1)
  var currentLeader = -1
  val leaders = ListBuffer.fill(graph.nodes.size)(-1)

  def depthFirstSearch(graph: Graph, startingPoint: Int): Unit = {
    explored(startingPoint - 1) = true
    leaders(startingPoint - 1) = currentLeader

    val newNodes = graph.edges.filter{
      case (src, dst) => src == startingPoint
    }.map{
      case (src, dst) => dst
    }

    for (node <- newNodes) {
      if (!explored(node - 1)) {
        depthFirstSearch(graph, node)
      }
    }

    numberNodesProcessedSoFar += 1
    finishingTimes(startingPoint - 1) = numberNodesProcessedSoFar
  }

  // Loop over all nodes in graph, running DFS
  for (i <- order.zipWithIndex.sortBy(_._1).reverse.map(_._2 + 1)) {
    if (!explored(i - 1)) {
      currentLeader = i
      depthFirstSearch(graph, i)
    }
  }

  (finishingTimes.toList, leaders.toList)
}


def stronglyConnectedComponents(graph: Graph): List[List[Int]] = {

  val graphReverse = reverseGraph(graph)
  val (order, _) = depthFirstSearchLoop(graphReverse, (graphReverse.nodes.max to graphReverse.nodes.min by -1).toList)
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
val testOneInput = "1 4\n2 8\n3 6\n4 7\n5 2\n6 9\n7 1\n8 5\n8 6\n9 7\n9 3".split("\n").toList
val testOneOutput = "3,3,3,0,0"

getSccSize(stronglyConnectedComponents(loadGraph(testOneInput))) == testOneOutput

val testTwoInput = "1 2\n2 6\n2 3\n2 4\n3 1\n3 4\n4 5\n5 4\n6 5\n6 7\n7 6\n7 8\n8 5\n8 7".split("\n").toList
val testTwoOutput = "3,3,2,0,0"

getSccSize(stronglyConnectedComponents(loadGraph(testTwoInput))) == testTwoOutput

val testThreeInput = "1 2\n2 3\n3 1\n3 4\n5 4\n6 4\n8 6\n6 7\n7 8".split("\n").toList
val testThreeOutput = "3,3,1,1,0"

getSccSize(stronglyConnectedComponents(loadGraph(testThreeInput))) == testThreeOutput

val testFourInput = "1 2\n2 3\n3 1\n3 4\n5 4\n6 4\n8 6\n6 7\n7 8\n4 3\n4 6".split("\n").toList
val testFourOutput = "7,1,0,0,0"

getSccSize(stronglyConnectedComponents(loadGraph(testFourInput))) == testFourOutput

val testFiveInput = "1 2\n2 3\n2 4\n2 5\n3 6\n4 5\n4 7\n5 2\n5 6\n5 7\n6 3\n6 8\n7 8\n7 10\n8 7\n9 7\n10 9\n10 11\n11 12\n12 10".split("\n").toList
val testFiveOutput = "6,3,2,1,0"

getSccSize(stronglyConnectedComponents(loadGraph(testFiveInput))) == testFiveOutput


/**
  * Homework Questions
  */

val dataFileURL = "https://raw.githubusercontent.com/brandonmburroughs/designAndAnalysisOfAlgorithms/master/designAndAnalysisOfAlgorithms1/week4/scc.txt"

val source = scala.io.Source.fromURL(dataFileURL).getLines

//println(getSccSize(stronglyConnectedComponents(loadGraph(source))))