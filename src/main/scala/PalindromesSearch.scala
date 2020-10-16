import java.io.{File, FileOutputStream, PrintWriter}

import scala.collection.mutable
import scala.collection.mutable.MutableList

object PalindromesSearch extends App {

  var OUTPUT_FILE_NAME = "palindromes.csv"

  /*
    We check if a list of number form a palindrome by reversing the list and doing an equality check on the list
   */
  def palindromeCheck(numberList: Array[Byte]): Boolean = {
    numberList.toList == numberList.toList.reverse
  }

  /*
    A function that returns true if a number can become a palindrome.
    If given a List(1,1,2) it returns true since the contents of the list can be reorganized to form a palindrome
   */
  def canBecomeNumberPalindrome(list: mutable.ArrayBuffer[Int]): Boolean = {
    var map = mutable.Map(1 -> 0, 2 -> 0, 3 -> 0, 4 -> 0, 5 -> 0, 7 -> 0, 8 -> 0, 9 -> 0)
    var odd = 0
    for (item <- list) {
      map.update(item, map.getOrElse(item, 0) + 1)
    }
    map.values.foreach { item =>
      if (item % 2 == 1) {
        odd += 1
      }
    }
    if (odd > 1) {
      return false
    } else {
      return true
    }
  }

  /*
  We start with a base number n e.g. 6 and store create an array the size of the number.
  This is for the base case where all the items are 1.
  We use the indexes of the array as a tool to spread out the numbers we would need to add up to the n value.
  i.e. if n = 6, then an array of size 6 would have indexes of numbers up to 6.
  We store the state of the n value in the value reducedNum, we  store another value num which represent  the max index
  to reach in our iteration while reducing our index value.
  We incrementally store the results in a Mutable Array buffer- result, we convert the values to Byte to ease the
  memory load.
   */


  def findCombinations(arr: Array[Byte], index: Int, num: Int, reducedNum: Int, filter: Int, result: mutable.ArrayBuffer[Array[Byte]]): mutable.ArrayBuffer[Array[Byte]] = {
    if (reducedNum < 0) {
      return result
    }
    if (reducedNum == 0) {
      var mutList = mutable.ArrayBuffer[Int]()
      for (i <- 0 until index) {
        mutList.+=:(arr(i))
      }
      if (canBecomeNumberPalindrome(mutList)) {
        if (mutList.contains(filter)) {
          result.+=:(mutList.toArray.map(_.toByte))
        }
      }
      return result
    }
    val prev = if (index == 0) 1
    else arr(index - 1)
    for (k <- prev to num) { // next element of
      // array is k
      arr.update(index, k.toByte)

      findCombinations(arr, index + 1, num, reducedNum - k, filter, result)
    }
    return result
  }


  def findCombinations(n: Int, m: Int): mutable.ArrayBuffer[List[Int]] = {
    // array to store the combinations
    // It can contain max n elements

    val arr = Array.ofDim[Byte](n)
    var resultList = mutable.ArrayBuffer.empty[Array[Byte]]
    // find all combinations

    return findCombinations(arr, 0, n, n, m, resultList)
      .tail
      .map(_.permutations.filter(palindromeCheck(_)))
      .flatten
      .map(_.toList.map(_.toInt))


  }

  def writeToFile(filename: String, value: mutable.ArrayBuffer[List[Int]]): Unit = {
    val pw = new PrintWriter(new FileOutputStream(new File(filename), false))
    for (lineList <- value) {
      var line = lineList.mkString(",")
      pw.write(s"$line\n")
    }
    pw.close
  }

  def startMain(): Unit = {
    var shouldSaveToFile = false
    var n = 0
    var m = 0
    println(
      "Welcome to the palindromic sequence project!"
    )
    var userInput = readLine().trim.split("\\s+")
    var arguments = userInput.toList
    if (arguments.size == 0) {
      println("Use: java PalindromesSearch n m [y]\n[y]: when informed, all palindromic sequences must be saved to a file")
      System.exit(1)
    }
    if (arguments.size == 3) {
      n = arguments(0).toInt
      m = arguments(1).toInt
      var arg3 = arguments(2).toString.toLowerCase()
      if (arg3 == "y") {
        shouldSaveToFile = true
        println("Generating palindromic sequences...")
        writeToFile(OUTPUT_FILE_NAME, findCombinations(n, m))
        println("Done!")
      }
    }
    if (arguments.size == 2) {
      n = arguments(0).toInt
      m = arguments(1).toInt
      println(s"Parameter n = $n\nParameter m = $m")
      var startTime = System.currentTimeMillis()
      var total = findCombinations(n, m).size
      var endTime = System.currentTimeMillis()
      println(s"Number of palindromic sequences found: $total\nIt took me ${(endTime - startTime) / 1000}s")
    }
  }

  startMain()
}
