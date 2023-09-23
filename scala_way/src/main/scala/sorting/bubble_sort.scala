package sorting

import scala.annotation.tailrec

class bubble_sort {

  /*
      [2, 6, 9, 1, 5]

      since we need to compare first two elements, so its better to split the list into 3 parts, first element, second, rest

      we need to traverse the whole list n(size of original list) times

      so we take a placeholder list, and everytime we are done with a fullscan of the list, we reduce its size, signifying one iteration is done

      source [2, 6, 9, 1, 5]
              h1 h2 rest

       h1 < h2, so we need to only, work with [6, 9, 1, 5], save 2 in result list

       source [6, 9, 1, 5]       result: [2]
               h1 h2 rest

      h1 < h2

      source [9, 1, 5]          result: [6, 2]
              h1 h2 rest

      h1 > h2, so 1 needs to be moved to head and we need to process [9, 5], save 1 in result

      source  [9, 5]            result: [1, 6, 2]
               h1 h2 rest

      h1 > h2

      source [9]               result: [5, 1, 6, 2]
              h1 h2 rest

     source has only one element, so its the largest in the list, push it to result

     source []                result: [9, 1, 6, 2]

     source is empty, but the placeholder list isn't which means more scans of the list are to be made
     now, result contains the list which is to be scanned in reverse order, so reversed result becomes
     source, reduce the size of placeholder list by 1, and make result empty again

     in the end, source and placeholder will be empty, then result will contain sorted list in reverse
     order, reverse it and good to go!

     Time Complexity: O(n^3) (since we need to reverse the result while commencing an action)

  */


  def sort(elems: List[Int]): List[Int] = {

    @tailrec
    def recurse(iterationCounterList: List[Int], source: List[Int], result: List[Int]): List[Int] = {

      if(source.isEmpty && iterationCounterList.isEmpty) result.reverse

      else if(source.isEmpty) recurse(iterationCounterList.dropRight(1), result.reverse, Nil)

      else if (source.tail.isEmpty) recurse(iterationCounterList, source.tail, source.head :: result)

      else if (source.head <= source.tail.head) recurse(iterationCounterList, source.tail, source.head :: result)

      else recurse(iterationCounterList, source.head :: source.tail.tail, source.tail.head :: result)
    }

    @tailrec
    def recurseBetterFormatted(iterationCounterList: List[Int], source: List[Int], result: List[Int]): List[Int] = {

      source match {
        case h1 :: h2 :: rest if(h1 <= h2) => recurseBetterFormatted(iterationCounterList, h2 :: rest, h1 :: result)
        case h1 :: h2 :: rest => recurseBetterFormatted(iterationCounterList, h1 :: rest, h2 :: result)
        case h1 :: Nil => recurseBetterFormatted(iterationCounterList, Nil, h1 :: result)
        case Nil  if(iterationCounterList.isEmpty) => result.reverse
        case Nil => recurseBetterFormatted(iterationCounterList.dropRight(1), result.reverse, Nil)
      }
    }

    recurseBetterFormatted(elems, elems, Nil)
  }
}

object bubble_sort extends App {
  println(new bubble_sort().sort(List(2, 6, 9, 1, 5)))
}
