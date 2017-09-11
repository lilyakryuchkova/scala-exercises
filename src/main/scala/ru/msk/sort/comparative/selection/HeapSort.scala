package ru.msk.sort.comparative.selection

import ru.msk.sort.quickcheck.SortTestCommand

import scala.annotation.tailrec

trait HeapSort[T] extends SortTestCommand[T]{

  // Worst case performance   О(n * log n)
  // Best-case performance    О(n * log n)
  // Average performance      О(n * log n)

  private def iParent(i: Int): Int = (i - 1) / 2
  private def iLeftChild(i: Int): Int = 2 * i + 1
  private def iRightChild(i: Int): Int = 2 * i + 2

  /*
  * Repair the heap whose root element is at index 'start',
  * assuming the heaps rooted at its children are valid
  * */
  private def siftDown(ts: Seq[T], start: Int, end: Int): Seq[T] = {

    @tailrec
    def helper(ys: Seq[T], root: Int = start): Seq[T] = {
      val leftChild = iLeftChild(root)
      val rightChild = leftChild + 1
      val isRightChildValid = rightChild <= end

      if (leftChild > end) ys
      else {
        // if root is greater or equal then children
        if ((ord gteq (ys(root), ys(leftChild))) &&
          (isRightChildValid && (ord gteq (ys(root), ys(rightChild))) ||
          !isRightChildValid)
        )
          ys

        // if root is less then leftChild
        else if ((ord lt (ys(root), ys(leftChild))) &&
          (isRightChildValid && (ord gteq(ys(leftChild), ys(rightChild))) ||
            !isRightChildValid))

            helper(
            swap(ys, root, leftChild),
            leftChild)

        // if root is less then rightChild
          else
            helper(
              swap(ys, root, rightChild),
              rightChild)
      }
    }

    helper(ts)
  }

  /*
  * Put elements of 'xs' in heap order
  * */
  private def heapify (xs: Seq[T], count: Int): Seq[T] = {

    /*
    * Put elements of 'a' in heap order, in-place
    * (start is assigned the index in 'xs' of the last parent node)
    * (the last element in a 0-based array is at index count-1; find the parent of that element)
    * */
    @tailrec
    def helper(ys: Seq[T], start: Int = iParent(count-1)): Seq[T] = {
      if (start < 0) ys
      else {
        // sift down the node at index 'start' to the proper place such that all nodes below
        // the start index are in heap order
        val partiallyHeaped = siftDown(ys, start, count - 1)
        // go to the next parent node
        helper(partiallyHeaped, start - 1)
      }
    }

    helper(xs)
  }

  private def heapsort(xs: Seq[T]): Seq[T] = {
    val count = xs size
    // Build the heap in array a so that largest value is at the root
    val heapified = heapify(xs, count)

    def helper(ys: Seq[T], end: Int = count - 1): Seq[T] = {
      if (end == 0) ys
      else {
        // The root is the largest value. Moves it in front of the sorted elements, e.g. to the end
        val unsorted = swap(ys, 0, end)
        // the move ruined the heap property, so restore it
        helper(siftDown(unsorted, 0, end - 1), end - 1)
      }
    }

    helper(heapified)
  }

  private def swap(xs: Seq[T], i: Int, j: Int): Seq[T] =
    xs updated(j, xs(i)) updated(i, xs(j))

  override def sort(xs: Seq[T]): Seq[T] = {
    heapsort(xs toVector)
  }
}
