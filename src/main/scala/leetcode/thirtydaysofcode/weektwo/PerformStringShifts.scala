package leetcode.thirtydaysofcode.weektwo

//You are given a string s containing lowercase English letters, and a matrix shift, where shiftArray(i) = Array(direction, amount):
//
//direction can be 0 (for left shift) or 1 (for right shift).
//amount is the amount by which string s is to be shifted.
//A left shift by 1 means remove the first character of s and append it to the end.
//Similarly, a right shift by 1 means remove the last character of s and add it to the beginning.
//Return the final string after all operations.
//
//
//
//Example 1:
//
//Input: s = "abc", shift = Array(Array(0,1),Array(1,2))
//Output: "cab"
//Explanation:
//Array(0,1) means shift to left by 1. "abc" -> "bca"
//Array(1,2) means shift to right by 2. "bca" -> "cab"
//Example 2:
//
//Input: s = "abcdefg", shift = Array(Array(1,1),Array(1,1),Array(0,2),Array(1,3))
//Output: "efgabcd"
//Explanation:
//Array(1,1) means shift to right by 1. "abcdefg" -> "gabcdef"
//Array(1,1) means shift to right by 1. "gabcdef" -> "fgabcde"
//Array(0,2) means shift to left by 2. "fgabcde" -> "abcdefg"
//Array(1,3) means shift to right by 3. "abcdefg" -> "efgabcd"


// "abcd" - 2 to the left -> "cdab"
// "abcd" - 2 to the right -> "cdab"

object PerformStringShifts extends App {

    def stringShift(s: String, shift: Array[Array[Int]]): String = {
        val movesSummed = shift.groupBy(_(0)).mapValues(row => row.map(_ (1)).sum)
        val moveToLeft = movesSummed.getOrElse(0, 0) - movesSummed.getOrElse(1, 0)

        val moveIndices = Math.abs(if (moveToLeft > 0) moveToLeft % s.length else s.length - ((moveToLeft * (-1)) % s.length ))

        s.slice(moveIndices, s.length) + s.slice(0, moveIndices)
    }


    println(stringShift("yzeuobhwxatulpruiab",
      Array(Array(1,15),Array(0,18),Array(0,12),Array(0,7),Array(0,7),Array(1,17),Array(1,15),Array(0,9),Array(1,4),Array(0,19),Array(1,16),Array(0,7),Array(1,4),Array(1,17),Array(1,19),Array(1,10),Array(1,2),Array(0,18),Array(1,15))
    ))

    println(stringShift("abc", Array(
      Array(0,1),
      Array(1,2)
    )))

    println(stringShift("blah", Array(
      Array(1,1),
      Array(1,1),
      Array(0,2),
      Array(1,3)
    )))

}
