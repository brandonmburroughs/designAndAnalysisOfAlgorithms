import scala.collection.mutable.ListBuffer
import scala.math.Ordering

/**
  * Sort an array with quicksort partitioning on the first element.
  * Notice that this implementation allows you to pass your pivot choosing
  * function as a parameter to allow for easier reusability
  *
  * @param choosePivot A function that returns a pivot and its index from a given List
  * @param input A list
  * @param ord (implicit) An ordering that provides a "less than" comparison operator
  * @return A sorted array and a count of comparisons made in the sorting process
  */
def quickSort[T](choosePivot: List[T] => (T, Int))(input: List[T])(implicit ord: Ordering[T]): (List[T], Long) = {

  // Parition subroutine
  def partition(input: List[T], pivot: T)(implicit ord: Ordering[T]): (List[T], T, List[T]) = {
    // We know that the pivot will be the 0th element
    var pivotPlacement = 1
    val partitionedList = input.to[ListBuffer]

    // Iterate through elements
    for (j <- 1 until partitionedList.length) {
      // If the current value is less than the pivot
      if (ord.lt(partitionedList(j), pivot)) {
        // Move it to the pivot position
        val swapValue = partitionedList(pivotPlacement)
        partitionedList(pivotPlacement) = input(j)
        partitionedList(j) = swapValue
        // Increase the partition placement values
        pivotPlacement = pivotPlacement + 1
      }
    }

    // Swap the pivot into its palce
    partitionedList(0) = partitionedList(pivotPlacement - 1)
    partitionedList(pivotPlacement - 1) = pivot

    // Split these to return two separate lists
    val (leftPartition, rightPartition) = partitionedList.toList splitAt (pivotPlacement - 1)

    (leftPartition, rightPartition.head, rightPartition.tail)
  }

  // Main routine
  val n = input.length

  if (n <= 1) (input, 0)
  else {
    val (pivot, pivotIndex) = choosePivot(input)

    // Ensure the pivot is in the front
    val inputPivotAtFrontTemp = input.to[ListBuffer]
    inputPivotAtFrontTemp(pivotIndex) = inputPivotAtFrontTemp.head
    inputPivotAtFrontTemp(0) = pivot
    val inputPivotAtFront = inputPivotAtFrontTemp.toList

    // Parition aroudn the pivot
    val (leftPartition, _, rightPartition) = partition(inputPivotAtFront, pivot)

    // Sort each pivot
    val (sortedLeftPartition, comparisonsLeft) = quickSort[T](choosePivot)(leftPartition)
    val (sortedRightPartition, comparisonsRight) = quickSort[T](choosePivot)(rightPartition)

    // Combine everything
    (sortedLeftPartition ++ List(pivot) ++ sortedRightPartition, (n - 1) + comparisonsLeft + comparisonsRight)
  }
}


// Type tests
quickSort[String](l => (l.head, 0))(List("caa","cab","aac","baa","cac"))
quickSort[Char](l => (l.head, 0))(List('c','d','j','k','a','b','z','q'))


// Pivot functions
def choosePivotFirstElement[T](input: List[T]): (T, Int) = (input.head, 0)

def choosePivotLastElement[T](input: List[T]): (T, Int) = (input.last, input.length - 1)

def choosePivotMedian[T](input: List[T])(implicit ord: Ordering[T]): (T, Int) = {
  val n = input.length
  val first = input.head
  val last = input.last
  val mid = input((n-1)/2)

  (ord.lt(first, mid), ord.lt(mid, last), ord.lt(first, last)) match {
    case (true, true, _) => (mid, (n-1)/2) //
    case (true, false, true) => (last, n-1)
    case (true, false, false) =>(first, 0)
    case (false, true, true) => (first, 0)
    case (false, true, false) => (last, n-1)
    case (false, false, _) => (mid, (n-1)/2)
  }
}


// Test cases
val tenTest = scala.io.Source.fromURL("https://dl.dropboxusercontent.com/u/20888180/AlgI_wk2_testcases/10.txt").getLines.toList.map(_.toInt)
val oneHundredTest = scala.io.Source.fromURL("https://dl.dropboxusercontent.com/u/20888180/AlgI_wk2_testcases/100.txt").getLines.toList.map(_.toInt)
val oneThousandTest = scala.io.Source.fromURL("https://dl.dropboxusercontent.com/u/20888180/AlgI_wk2_testcases/1000.txt").getLines.toList.map(_.toInt)


// Tests for choosing first element as pivot
quickSort(choosePivotFirstElement[Int])(tenTest) == (1 to 10, 25)
quickSort(choosePivotFirstElement[Int])(oneHundredTest) == (1 to 100, 615)
quickSort(choosePivotFirstElement[Int])(oneThousandTest) == (1 to 1000, 10297)


// Tests for choosing last element as pivot
quickSort(choosePivotLastElement[Int])(tenTest) == (1 to 10, 29)
quickSort(choosePivotLastElement[Int])(oneHundredTest) == (1 to 100, 587)
quickSort(choosePivotLastElement[Int])(oneThousandTest) == (1 to 1000, 10184)


// Tests for choosing median element as pivot
quickSort(choosePivotMedian[Int])(tenTest) == (1 to 10, 21)
quickSort(choosePivotMedian[Int])(oneHundredTest) == (1 to 100, 518)
quickSort(choosePivotMedian[Int])(oneThousandTest) == (1 to 1000, 8921)

// Read file
val source = scala.io.Source.fromURL("https://raw.githubusercontent.com/brandonmburroughs/designAndAnalysisOfAlgorithms/master/designAndAnalysisOfAlgorithms1/week2/QuickSort.txt")

// Convert to array
val inputList = source.getLines.toList.map(_.toInt)

// Homework Questions
val (sortedInputList1, countComparisons1) = quickSort(choosePivotFirstElement[Int])(inputList)
val (sortedInputList2, countComparisons2) = quickSort(choosePivotLastElement[Int])(inputList)
val (sortedInputList3, countComparisons3) = quickSort(choosePivotMedian[Int])(inputList)
println(s"The counts of comparisons for three different pivot methods are $countComparisons1, $countComparisons2, and $countComparisons3!")