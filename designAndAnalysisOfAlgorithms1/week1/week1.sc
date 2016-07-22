import math.Ordering

/**
  * Split and conquer method to count the number of inversions in the given list of elements.
  * An inversion is when a larger number precedes a smaller number.
  *
  * @param input A list
  * @param ord (implicit) An ordering that provides a "less than" comparison operator
  * @return An integer count of inversions
 */
def sortAndCountInversions[T](input: List[T])(implicit ord: Ordering[T]): (List[T], Long) = {
  val n = input.length

  def mergeAndCountSplitInv[T](leftInput: List[T], rightInput: List[T])(implicit ord: Ordering[T]): (List[T], Long) = {
    val ln = leftInput.length
    val rn = rightInput.length
    var i = 0
    var j = 0
    val sortedList = scala.collection.mutable.ListBuffer.empty[T]
    var splitInversions = 0
    for (k <- 0 until (ln + rn)) {
      if (j == rn && i < ln) {
        sortedList += leftInput(i)
        i = i + 1
      } else if (i == ln && j < rn) {
        sortedList += rightInput(j)
        j = j + 1
      } else if (ord.lt(leftInput(i), rightInput(j)) && i < ln) {
        sortedList += leftInput(i)
        i = i + 1
      } else {
        sortedList += rightInput(j)
        j = j + 1
        splitInversions = splitInversions + (ln - i)
      }
    }

    (sortedList.toList, splitInversions)
  }


  if (n == 1) (input, 0)
  else {
    val (b, x) = sortAndCountInversions(input.slice(0,n/2))
    val (c, y) = sortAndCountInversions(input.slice(n/2, n))
    val (d, z) = mergeAndCountSplitInv(b, c)

    (d, x + y + z)
  }
}

// Tests
sortAndCountInversions(List(1,2,3,4,5,6)) == (List(1,2,3,4,5,6), 0)
sortAndCountInversions(List(1,3,5,2,4,6)) == (List(1,2,3,4,5,6), 3)
sortAndCountInversions(List(6,5,4,3,2,1)) == (List(1,2,3,4,5,6), 15)


/*
 * Split and conquer method to count the number of inversions in the given list of elements.
 * An inversion is when a larger number precedes a smaller number.  This implementation follows
 * a more Scalaesque style.
 *
 * @param A List
 * @returns An integer count of inversions
 *
 * Note: This causes a stack overflow error on large input arrays.
 */
def sortAndCountInversionsScalaesque[T](xs: List[T])(implicit ord: Ordering[T]): (List[T], Long) = {
  val n = xs.length / 2
  if (n == 0) (xs, 0)
  else {
    def mergeAndCountSplitInv(xs: List[T], ys:List[T]): (List[T], Long) = {
      (xs, ys) match {
        case (Nil, ys) => (ys, 0)
        case (xs, Nil) => (xs, 0)
        case (x :: xs1, y :: ys1) =>
          if (ord.lt(x , y)) {
            val (tail, count) = mergeAndCountSplitInv(xs1, ys)
            (x :: tail, count)
          } else {
            val (tail, count) = mergeAndCountSplitInv(xs, ys1)
            (y :: tail, xs.length + count)
          }
      }
    }

    val (fst, snd) = xs splitAt n
    val (fstSorted, fstCount) = sortAndCountInversionsScalaesque(fst)
    val (sndSorted, sndCount) = sortAndCountInversionsScalaesque(snd)
    val (mergedList, splitInversionCount) = mergeAndCountSplitInv(fstSorted, sndSorted)

    (mergedList, fstCount + sndCount + splitInversionCount)
  }
}


// Tests
sortAndCountInversionsScalaesque(List(1,2,3,4,5,6)) == (List(1,2,3,4,5,6), 0)
sortAndCountInversionsScalaesque(List(1,3,5,2,4,6)) == (List(1,2,3,4,5,6), 3)
sortAndCountInversionsScalaesque(List(6,5,4,3,2,1)) == (List(1,2,3,4,5,6), 15)


// Read file
val source = scala.io.Source.fromURL("https://raw.githubusercontent.com/brandonmburroughs/designAndAnalysisOfAlgorithms/master/designAndAnalysisOfAlgorithms1/week1/input_array.txt")

// Convert to array
val inputList = source.getLines.toList.map(_.toInt)

// Homework Question 1
val (sortedInputList, inputListSplitInversionCount) = sortAndCountInversions(inputList)
println(s"There are $inputListSplitInversionCount split inversions in this array!")