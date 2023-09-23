package sorting

import scala.annotation.tailrec

/*
  logic

  [5, 1, 2, 3, 6]

  Algo
  1st iteration - [5, 1] (1 < 5)
  [1, 5]
  [1, 5, 2, 3, 6]

  2nd iteration [1, 5, 2, 3, 6] (2 < 5)
  [1, 2, 5, 3, 6]

  Scala way of looking at things
  [5, 1, 2, 6, 3]        Result list: [] (contains elements in sorted order)
  insert 5 in result list  Result list: [5]
  [1, 2, 6, 3]  [5]
  insert 1 in result list
  1 < 5 so insert 1 at head
  [2, 6, 3]    [1, 5]
  insert 2 in result list
  2 > 1 so need to find its position in the tail of result list [5]
  2 < 5, so insert 2 before 5
  [6, 3]    [1, 2, 5]
  insert 6 in result list
  6 > 1, look in tail [2, 5]
  6 > 2, look in tail [5]
  6 > 5 look in tail []
  insert at head
  [3]               [1, 2, 5, 6]
  insert 3 in result list
  3 > 1 look in tail [2, 5, 6]
  3 > 2 look in tail [5, 6]
  3 < 5 so insert at head
  []                [1, 2, 3, 5, 6]


  so in order to do this in an immutable fashion, think of it as inserting a new element in a sorted list everytime
  also since inserting in a list is O(1) from head in scala, so start inserting from head

  also iteration is not preferred in scala (immutability violated), so do it recursively
*/

class insertion_sort {

  def sort(elems: List[Int]): List[Int] = {

    def insertInSortedListNonTailRec(elem: Int, sortedList: List[Int]): List[Int] = {
      if(sortedList.isEmpty) (elem :: sortedList)

      else if (elem  < sortedList.head) (elem :: sortedList)

      else sortedList.head :: insertInSortedListNonTailRec(elem, sortedList.tail)
    }

    @tailrec
    def insertInInSortedList(elem: Int, sortedList: List[Int], accum: List[Int]): List[Int] = {
        if(sortedList.isEmpty) accum.reverse ++ (elem :: sortedList)

        else if(elem > sortedList.head) insertInInSortedList(elem, sortedList.tail, sortedList.head :: accum)

        else accum.reverse ++ (elem :: sortedList)
    }

    @tailrec
    def recurse(elems: List[Int], result: List[Int]): List[Int] = {
      if(elems.isEmpty) result

      else {
        val updatedResult: List[Int] = insertInSortedListNonTailRec(elems.head, result)
        recurse(elems.tail, updatedResult)
      }
    }

    recurse(elems, Nil)
  }
}

object insertion_sort extends App {
   println(new insertion_sort().sort(List(5, 1, 2, 6, 3)))
}
