package sorting

import scala.annotation.tailrec

/*
  logic:
  merge sort is a divide and conquer technique where we split the array into two parts
  and recursively call on the small arrays
  finally we merge the two sorted arrays

                                                               [5, 2, 6, 1, 0]

                            left half: [5, 2, 6]                                               right half: [1, 0]
                                                                                        left result: [1]          right result: [0]
              left: [5, 2]                            right: [6] (already sorted)        left: [1]                right: [0]
     left result: [5]
     right result: [2]
     result array: [2, 5]


  left: [5](sorted)                   right: [2](sorted)






  the tail recursive logic
  function signature- (smallList: List[List[Int]], bigList: List[List[Int]]): List[Int]

  input: [3, 1, 2, 5, 4]
  split it into lists of single element
  1. smallList: [[3], [1], [2], [5], [4]] bigList: []
  2. call merge two sorted arrays on the first two elements of small list
     put result into bigList
     smallList: [[1, 3], [2], [5], [4]]   bigList: []
     smallList: [[2], [5], [4]]  bigList: [[1, 3]]
     smallList: [[2, 5], [4]]   bigList: [[1, 3]]
     smallList: [[4]]  bigList: [[2, 5], [1, 3]]
     since small list has only 1 element, put it into bigList
     smallList: [[4]]  bigList: [[2, 5], [1, 3]]
     smallList: []  bigList: [[4], [2, 5], [1, 3]]
     now small list is empty but bigList has more than 1 element, so swap the arguments
     smallList: [[4], [2, 5], [1, 3]]   bigList: []
     smallList: [[2, 4, 5], [1, 3]]  bigList: []
     smallList: [[1, 3]]   bigList: [[2, 4, 5]]
     smallList: []    bigList: [[1, 3], [2, 4, 5]]
     smallList: [[1, 3], [2, 4, 5]]   bigList: []
     smallList: [[1, 2, 3, 4 , 5]]     bigList: []
     smallList: []   bigList: [[1, 2, 3, 4, 5]]
     smallList is empty, and bigList only contains one element, so return that element
     result: [1, 2, 3, 4, 5]

*/
class merge_sort {

  // takes 2 sorted lists and merges them to give a sorted list
  private def mergeTwoSortedArrays(sortedList1: List[Int], sortedList2: List[Int]): List[Int] = {

    @tailrec
    def recurse(list1: List[Int], list2: List[Int], result: List[Int]): List[Int] = {
      (list1, list2) match {
        case (Nil, Nil) => result.reverse
        case (Nil, list2) => recurse(Nil, Nil, list2.reverse ++ result)
        case (list1, Nil) => recurse(Nil, Nil, list1.reverse ++ result)
        case (list1, list2) if (list1.head <= list2.head) => recurse(list1.tail, list2, list1.head :: result)
        case (list1, list2) => recurse(list1, list2.tail, list2.head :: result)
      }
    }

    recurse(sortedList1, sortedList2, List())
  }

  def sortNonTailRecursive(source: List[Int]): List[Int] = {

    def recurse(list: List[Int], result: List[Int]): List[Int] = {


      list match {
        case Nil => result.reverse
        case head :: Nil => recurse(Nil, head :: result)
        case _ =>
          val mid = list.length / 2
          val (leftHalf, rightHalf) = list.splitAt(mid)
          val leftSorted: List[Int] = recurse(leftHalf, result)
          val rightSorted: List[Int] = recurse(rightHalf, result)
          mergeTwoSortedArrays(leftSorted, rightSorted)
      }
    }

    recurse(source, Nil)
  }

  def sort(source: List[Int]): List[Int] = {
    @tailrec
    def recurse(smallList: List[List[Int]], bigList: List[List[Int]]): List[Int] = {
          if(smallList.isEmpty){
            if(bigList.isEmpty) Nil
            else if(bigList.tail.isEmpty) bigList.head
            else recurse(bigList, Nil)
          }

          else if(smallList.tail.isEmpty) {
            // if we have an input list with 1 element, then that is the answer itself
            // recursively calling lien 107 will lead to infinite recursion since we
            // recurse using the same state we started with
            if(bigList.isEmpty) smallList.head
            else recurse(smallList.head :: bigList, Nil)
          }

          else{
            val list1 = smallList.head
            val list2 = smallList.tail.head
            val sortedList = mergeTwoSortedArrays(list1, list2)
            recurse(smallList.tail.tail, sortedList::bigList)
          }
        }

    recurse(source.map(elem => List(elem)), Nil)
  }
}

object merge_sort extends App {
  println(new merge_sort().sort(List(5)))
}
