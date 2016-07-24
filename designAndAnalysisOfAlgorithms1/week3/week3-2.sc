// This is similar to the other implementation but returns a string representation of the clusters

import scala.util.Random

/**
  * Compute the minimum number of cuts when randomly cutting this graph
  * as well as the two clusters formed by the cuts.
  *
  * @param nodes A list of nodes in the graph, given by vertex ID
  * @param edges A list of edges, given as a tuple of vertex IDs
  * @return The global clusters as well as the minimum number of cuts.
  */
def minCut(nodes: List[Int], edges: List[(Int, Int)]): (List[(Int, Int)], Int, List[String]) = {

  def randomContraction(nodes: List[Int], edges: List[(Int, Int)], nodeClusters: List[String]): (List[(Int, Int)], List[String]) = {

    def contract(u: Int, v: Int, nodes: List[Int], edges: List[(Int, Int)]): (List[Int], List[(Int, Int)], List[String]) = {
      // Remove v from nodes
      val vIndex = nodes indexOf v
      val newNodes = (nodes take vIndex) ++ (nodes drop vIndex + 1)

      // Change "v" edges to "u" edges
      val newEdges = edges.map{
        case (src, dst) =>
          (src, dst) match {
            case (`v`, _) => (u, dst)
            case (_, `v`) => (src, u)
            case (_, _) => (src, dst)
          }
      }.filter{ // Remove self loops
        case (src, dst) => src != dst
      }

      // Get the new node clusters
      val vIndexCluster = nodeClusters indexOf nodeClusters.filter(node => node.contains(v.toString)).head
      val newNodeClusters = ((nodeClusters take vIndexCluster) ++ (nodeClusters drop vIndexCluster + 1)).map {
        case node =>
          if (node.contains(u.toString)) s"$node - ${nodeClusters(vIndexCluster)}"
          else node
      }

      // Return new graph
      (newNodes, newEdges, newNodeClusters)
    }

    // Main routine
    val rnd = new Random

    if (nodes.length == 2) (edges.filter{
      case (src, dst) => src == nodes.head
    }, nodeClusters)
    else {
      val (u, v) = edges(rnd.nextInt(edges.length))
      val (contractedNodes, contractedEdges, newNodeClusters) = contract(u, v, nodes, edges)
      randomContraction(contractedNodes, contractedEdges, newNodeClusters)
    }
  }

  var globalMinCuts = nodes.length + 1
  var globalMinEdges = edges
  var globalNodeClusters = List[String]()
  var i = 0
  while (i < nodes.length * 10) {
    val (newEdges, newNodeClusters) = randomContraction(nodes, edges, nodes.map(_.toString))
    if (newEdges.length < globalMinCuts) {
      globalMinCuts = newEdges.length
      globalMinEdges = newEdges
      globalNodeClusters = newNodeClusters
    }
    i += 1
  }

  (globalMinEdges, globalMinCuts, globalNodeClusters)
}


/**
  * Tests
  */

val testOneInput =
  """1 2 3 4 7
    |2 1 3 4
    |3 1 2 4
    |4 1 2 3 5
    |5 4 6 7 8
    |6 5 7 8
    |7 1 5 6 8
    |8 5 6 7""".stripMargin.split("\n").toList

val testOneOutput = 2

val testOneNodes = (1 to testOneInput.length).toList
val testOneEdges = testOneInput.flatMap{
  case lineString: String =>
    val line = lineString.split(' ').map(_.toInt)
    line.slice(1,line.length).map(r => (line(0), r))
}

minCut(testOneNodes, testOneEdges)//._2 == testOneOutput


/**
  * Homework Questions
  */

// Read file
val source = scala.io.Source.fromURL("https://raw.githubusercontent.com/brandonmburroughs/designAndAnalysisOfAlgorithms/master/designAndAnalysisOfAlgorithms1/week3/kargerMinCut.txt").getLines.toList

// Convert to array
val nodes = (1 to source.length).toList

val edges = source.flatMap{
  case lineString: String =>
    val line = lineString.split('\t').map(_.toInt)
    line.slice(1,line.length).map(r => (line(0), r))
}

// Homework Question 1
val (minEdges, minCuts, minNodeClusters) = minCut(nodes, edges)

println(s"The minimum number of cuts is $minCuts!")