import scala.collection.mutable.HashMap
import util.control.Breaks._

/**
  * Load a file with one number per line into a map
  *
  * @param fileURL The URL for the file location
  * @return a `Map[Long, Long]` number -> number mapping
  */
def loadMap(fileURL: String): Map[Long, Long] = {
  // Initialize map
  val map = HashMap[Long, Long]()

  // Get lines and iterate over them
  val lines = scala.io.Source.fromFile(fileURL).getLines.map(_.toLong)
  lines.foreach {
    line =>
      map(line) = line
  }

  map.toMap withDefaultValue Long.MaxValue
}


/**
  * Given a range of targets, find two numbers in the hash map that sum to one
  * of the targets.  Exclude duplicates (i.e. (x, y) and (y, x) only count as
  * one) and non-distinct values (i.e. (x,x)).
  *
  * @param map a `Map[Long, Long]` value map of numbers
  * @param targetFrom the minimum target range
  * @param targetTo the maximum target range
  * @return a `List[(Int, Int)]` list of pairs of values that sum to a value
  *         in the target range.
  */
def twoSumTargetRange(map: Map[Long, Long], targetFrom: Int, targetTo: Int): List[(Long, Long)] = {
  // Initialize counter
  var twoSums = List[(Long, Long)]()

  // Look through all values
  for (target <- targetFrom to targetTo) {
    breakable {
      for (key <- map.keys) {
        if (map(target - key) != Long.MaxValue && key != (target - key)) {
          twoSums = twoSums :+ (key, target - key)
          break
        }
      }
    }
  }

  // Filter out duplicates
  val twoSumsSorted = twoSums.map {
    case (keyOne, keyTwo) =>
      if (keyOne < keyTwo) (keyOne, keyTwo)
      else (keyTwo, keyOne)
  }

  twoSumsSorted.distinct
}


def medianStreamLessEfficient(numberList: List[Long]): List[Long] = {
  // Initialize median list
  var medianList = List[Long]()

  def median(numberListPiece: List[Long]): Long = {
    val n = numberListPiece.length

    if (n % 2 == 1) numberListPiece(((n + 1) / 2) - 1)
    else numberListPiece((n / 2) - 1)
  }

  for (i <- 1 to numberList.length) {
    medianList = medianList :+ median(numberList.slice(0, i).sorted)
  }

  medianList
}


def medianStream(numberList: List[Long]): List[Long] = {
  // Initialize median list
  var medianList = List[Long]()

  def median(numberListPiece: List[Long]): Long = {
    val n = numberListPiece.length

    if (n % 2 == 1) numberListPiece(((n + 1) / 2) - 1)
    else numberListPiece((n / 2) - 1)
  }

  for (i <- 1 to numberList.length) {
    medianList = medianList :+ median(numberList.slice(0, i).sorted)
  }

  medianList
}

/**
  * Test
  */
// Homework Question 1
val questionOneTestInputOneURL = "/Users/rwp651/Documents/capital_one/git/coursera/designAndAnalysisOfAlgorithms/designAndAnalysisOfAlgorithms1/week6/testCases/questionOneTestCaseOne.txt"//"https://raw.githubusercontent.com/brandonmburroughs/designAndAnalysisOfAlgorithms/master/designAndAnalysisOfAlgorithms1/week6/testCases/questionOneTestCaseOne.txt"
val questionOneTestOneMap = loadMap(questionOneTestInputOneURL)
val questionOneTestOneOutput = 3
val questionOneTestOneTwoSum = twoSumTargetRange(questionOneTestOneMap, -10000, 10000)
questionOneTestOneTwoSum.length == questionOneTestOneOutput

val questionOneTestInputTwoURL = "/Users/rwp651/Documents/capital_one/git/coursera/designAndAnalysisOfAlgorithms/designAndAnalysisOfAlgorithms1/week6/testCases/questionOneTestCaseTwo.txt"//"https://raw.githubusercontent.com/brandonmburroughs/designAndAnalysisOfAlgorithms/master/designAndAnalysisOfAlgorithms1/week6/testCases/questionOneTestCaseTwo.txt"
val questionOneTestTwoMap = loadMap(questionOneTestInputTwoURL)
val questionOneTestTwoOutput = 5
val questionOneTestTwoTwoSum = twoSumTargetRange(questionOneTestTwoMap, -10000, 10000)
questionOneTestTwoTwoSum.length == questionOneTestTwoOutput

// Homework Question 2
val questionTwoTestInputOneURL = "/Users/rwp651/Documents/capital_one/git/coursera/designAndAnalysisOfAlgorithms/designAndAnalysisOfAlgorithms1/week6/testCases/questionTwoTestCaseOne.txt"//"https://raw.githubusercontent.com/brandonmburroughs/designAndAnalysisOfAlgorithms/master/designAndAnalysisOfAlgorithms1/week6/testCases/questionTwoTestCaseOne.txt"
val questionTwoTestOneOutput = 9335
val questionTwoTestOneList = scala.io.Source.fromFile(questionTwoTestInputOneURL).getLines.map(_.toLong).toList
val questionTwoTestOneMediansLessEfficient = medianStreamLessEfficient(questionTwoTestOneList)
questionTwoTestOneMediansLessEfficient.sum % 10000 == questionTwoTestOneOutput

val questionTwoTestOneMedians = medianStream(questionTwoTestOneList)
questionTwoTestOneMedians.sum % 10000 == questionTwoTestOneOutput


/**
  * Homework Questions
  */

// Homework Question 1
val questionOneInputURL = "/Users/rwp651/Documents/capital_one/git/coursera/designAndAnalysisOfAlgorithms/designAndAnalysisOfAlgorithms1/week6/algo1-programming_prob-2sum.txt"//"https://raw.githubusercontent.com/brandonmburroughs/designAndAnalysisOfAlgorithms/master/designAndAnalysisOfAlgorithms1/week6/algo1-programming_prob-2sum.txt"
val questionOneMap = loadMap(questionOneInputURL)
val questionOneTwoSum = twoSumTargetRange(questionOneMap, -10000, 10000)
questionOneTwoSum.length

// Homework Question 2
val questionTwoInputURL = "/Users/rwp651/Documents/capital_one/git/coursera/designAndAnalysisOfAlgorithms/designAndAnalysisOfAlgorithms1/week6/Median.txt"//"https://raw.githubusercontent.com/brandonmburroughs/designAndAnalysisOfAlgorithms/master/designAndAnalysisOfAlgorithms1/week6/Median.txt"
val questionTwoList = scala.io.Source.fromFile(questionTwoInputURL).getLines.map(_.toLong).toList
val questionTwoSum = medianStream(questionTwoList)
questionTwoSum.sum % 10000
