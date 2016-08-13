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
  val lines = scala.io.Source.fromURL(fileURL).getLines.map(_.toLong)
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
        if (map(target - key) != Long.MaxValue && key != (target - key) && key < (target - key)) {
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

/**
  * Given a range of targets, find two numbers in the hash map that sum to one
  * of the targets.  Exclude duplicates (i.e. (x, y) and (y, x) only count as
  * one) and non-distinct values (i.e. (x,x)).
  *
  * Note:  This may be faster when looking through a large target range.  This
  * exploits the sorted order of the List.
  *
  * @param nums a `List[Long]` list of numbers
  * @param targetFrom the minimum target range
  * @param targetTo the maximum target range
  * @return a `List[(Int, Int)]` list of pairs of values that sum to a value
  *         in the target range.
  */
def twoSumTargetRangeList(nums: List[Long], targetFrom: Int, targetTo: Int): List[(Long, Long)] = {
  // Initialize counter
  var twoSums = List[(Long, Long)]()
  var targetsFound = List[Int]()

  // Sort the list
  val numsSorted = nums.sorted

  // Look through list
  for (i <- numsSorted.indices) {
    // Always start looking at the next element
    var j = i + 1

    // Since the array is sorted, we don't need to look past a certain index
    while (j < numsSorted.length && targetTo - numsSorted(i) > numsSorted(j)) {

      // Consider all targets
      for (target <- (targetFrom to targetTo).toList diff targetsFound) {

        // Two sum match
        if (numsSorted(i) + numsSorted(j) == target && numsSorted(i) != numsSorted(j)) {
          twoSums = twoSums :+ (numsSorted(i), numsSorted(j))
          targetsFound = targetsFound :+ target
        }
      }

      j += 1
    }
  }

  twoSums.distinct
}


/**
  * Given a list of numbers, calculate the median with the addition of each new
  * number, as if they were being streamed in.
  *
  * Note:  This lazy version uses the `List.sorted` method directly, which
  * would cause this to run slower than potentially possible.
  *
  * @param numberList the list of all numbers
  * @return A list of medians, positioned in the list for the addition of a
  *         particular number, e.g. the fourth element will be the median
  *         when there were four elements in the list
  */
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


/**
  * Given a list of numbers, calculate the median with the addition of each new
  * number, as if they were being streamed in.
  *
  * Note:  This version keeps track of the median using stacks and can run in
  * linear time.
  *
  * @param numberList the list of all numbers
  * @return A list of medians, positioned in the list for the addition of a
  *         particular number, e.g. the fourth element will be the median
  *         when there were four elements in the list
  */
// TODO:  convert to running median using stacks
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
  * Tests
  */
// Homework Question 1
val questionOneTestInputOneURL = "https://raw.githubusercontent.com/brandonmburroughs/designAndAnalysisOfAlgorithms/master/designAndAnalysisOfAlgorithms1/week6/testCases/questionOneTestCaseOne.txt"
val questionOneTestOneMap = loadMap(questionOneTestInputOneURL)
val questionOneTestOneOutput = 3
val questionOneTestOneTwoSum = twoSumTargetRange(questionOneTestOneMap, -10000, 10000)
questionOneTestOneTwoSum.length == questionOneTestOneOutput

// List
val questionOneTestOneLines = scala.io.Source.fromURL(questionOneTestInputOneURL).getLines.map(_.toLong).toList
twoSumTargetRangeList(questionOneTestOneLines, -10000, 1000).length == questionOneTestOneOutput


val questionOneTestInputTwoURL = "https://raw.githubusercontent.com/brandonmburroughs/designAndAnalysisOfAlgorithms/master/designAndAnalysisOfAlgorithms1/week6/testCases/questionOneTestCaseTwo.txt"
val questionOneTestTwoMap = loadMap(questionOneTestInputTwoURL)
val questionOneTestTwoOutput = 5
val questionOneTestTwoTwoSum = twoSumTargetRange(questionOneTestTwoMap, -10000, 10000)
questionOneTestTwoTwoSum.length == questionOneTestTwoOutput

// List
val questionOneTestTwoLines = scala.io.Source.fromURL(questionOneTestInputTwoURL).getLines.map(_.toLong).toList
twoSumTargetRangeList(questionOneTestTwoLines, -10000, 1000).length == questionOneTestTwoOutput


// Homework Question 2
val questionTwoTestInputOneURL = "https://raw.githubusercontent.com/brandonmburroughs/designAndAnalysisOfAlgorithms/master/designAndAnalysisOfAlgorithms1/week6/testCases/questionTwoTestCaseOne.txt"
val questionTwoTestOneOutput = 9335
val questionTwoTestOneList = scala.io.Source.fromURL(questionTwoTestInputOneURL).getLines.map(_.toLong).toList
val questionTwoTestOneMediansLessEfficient = medianStreamLessEfficient(questionTwoTestOneList)
questionTwoTestOneMediansLessEfficient.sum % 10000 == questionTwoTestOneOutput

val questionTwoTestOneMedians = medianStream(questionTwoTestOneList)
questionTwoTestOneMedians.sum % 10000 == questionTwoTestOneOutput


/**
  * Homework Questions
  */

// Homework Question 1
val questionOneInputURL = "https://raw.githubusercontent.com/brandonmburroughs/designAndAnalysisOfAlgorithms/master/designAndAnalysisOfAlgorithms1/week6/algo1-programming_prob-2sum.txt"
val questionOneMap = loadMap(questionOneInputURL)
val questionOneTwoSum = twoSumTargetRange(questionOneMap, -10000, 10000)
questionOneTwoSum.length

val questionOneLines = scala.io.Source.fromURL(questionOneInputURL).getLines.map(_.toLong).toList
twoSumTargetRangeList(questionOneLines, -10000, 1000).length

// Homework Question 2
val questionTwoInputURL = "https://raw.githubusercontent.com/brandonmburroughs/designAndAnalysisOfAlgorithms/master/designAndAnalysisOfAlgorithms1/week6/Median.txt"
val questionTwoList = scala.io.Source.fromURL(questionTwoInputURL).getLines.map(_.toLong).toList
val questionTwoSum = medianStream(questionTwoList)
questionTwoSum.sum % 10000
