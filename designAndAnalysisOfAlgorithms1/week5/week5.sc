import scala.collection.mutable.HashMap

/**
  * Load a graph into a source vertex -> (destination vertex, edge weight) map.
  *
  * @param fileURL The URL for the file location
  * @return a `Map[Int, List[(Int, Int)]]` source vertex -> (destination vertex, edge weight) map
  */
def loadWeightedGraph(fileURL: String): Map[Int, List[(Int, Int)]] = {
  val graph = HashMap[Int, List[(Int, Int)]]() withDefaultValue Nil

  val lines = scala.io.Source.fromURL(fileURL).getLines
  lines.foreach{
    line =>
      val lineSplit = line.split("\t").map(_.trim)
      val (src, dstNodes) = (lineSplit.head.toInt, lineSplit.tail)
      dstNodes.foreach{
        node =>
          val nodeSplit = node.split(",").map(_.toInt)
          val (dst, weight) = (nodeSplit.head, nodeSplit.tail.head)
          graph(src) = graph(src) :+ (dst, weight)
      }
  }
  graph.toMap withDefaultValue Nil
}

/**
  * Dijkstra's Shorted Path Algorithm
  * Given a graph and a starting point, this algorithm computes a shortest path distance
  * between the starting point and all connected edges.  For all other edges, a value of
  * `Int.MaxValue` will be assigned.
  *
  * @param graph a `Map[Int, List[(Int, Int)]]`, srcVertex -> List[(dstVertex, weight)] `Map` representation of our graph
  * @param startingVertex an `Int` representing the starting node
  * @return a `Map[Int, Int]`, vertex -> shortestPath `Map` with all shortest paths
  */
def dijkstraShortestPathAllVertices(graph: Map[Int, List[(Int, Int)]], startingVertex: Int): Map[Int, Int] = {
  // Initialize bookkeeping
  var visitedNodes = Set[Int](startingVertex)
  val shortestPaths = HashMap[Int, Int](startingVertex -> 0) //withDefaultValue 0

  while (visitedNodes != graph.keys) {
    // Initialize shortes paths
    var shortestPathDistance = 10000000
    var shortestPathDst = startingVertex

    // Look through all nodes that have been already visited
    for (src <- visitedNodes) {
      // Get all edges associated with a visited node
      val dstList = graph(src).filter {
        case (dst, weight) => !visitedNodes.contains(dst)
      }

      // Look for shortest edge that crosses boundary
      for ((dst, weight) <- dstList) {
        val newDistance = shortestPaths(src) + weight
        if (newDistance < shortestPathDistance) {
          shortestPathDistance = newDistance
          shortestPathDst = dst
        }
      }
    }

    // Updates with short path node
    visitedNodes = visitedNodes + shortestPathDst
    shortestPaths(shortestPathDst) = shortestPathDistance
  }

  shortestPaths.toMap withDefaultValue 0
}

def getHomeworkOutput(shortestPaths: Map[Int, Int]): String = {
  val verticesNeeded = List(7, 37, 59, 82, 99, 115, 133, 165, 188, 197)
  var nodes = List[Int]()
  verticesNeeded.foreach { vertex =>
    nodes = nodes :+ shortestPaths(vertex)
  }

  nodes.mkString(",")
}


/**
  * Tests
  */
// Data
val testOneInputFilePath = "https://raw.githubusercontent.com/brandonmburroughs/designAndAnalysisOfAlgorithms/master/designAndAnalysisOfAlgorithms1/week5/testCases/one.txt"
val testOneOutput = Map(1 -> 0, 2 -> 1, 3 -> 2, 4 -> 3, 5 -> 4, 6 -> 4, 7 -> 3, 8 -> 2)

// Get graph and shortes paths
val testOneGraph = loadWeightedGraph(testOneInputFilePath)
val testOneShortestPaths = dijkstraShortestPathAllVertices(testOneGraph, 1)

// Check
testOneShortestPaths == testOneOutput


/**
  * Homework Questions
  */
// Data
val homeworkInputFilePath = "https://raw.githubusercontent.com/brandonmburroughs/designAndAnalysisOfAlgorithms/master/designAndAnalysisOfAlgorithms1/week5/dijkstraData.txt"

// Get graph and shortes paths
val homeworkGraph = loadWeightedGraph(homeworkInputFilePath)
val homeworkShortestPaths = dijkstraShortestPathAllVertices(homeworkGraph, 1)

// Compute shortest paths
getHomeworkOutput(homeworkShortestPaths)