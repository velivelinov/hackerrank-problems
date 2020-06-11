package others


//Given a string, find the length of the longest substring without repeating characters.
//
//Example 1:
//
//Input: "abcabcbb"
//Output: 3
//Explanation: The answer is "abc", with the length of 3.
//Example 2:
//
//Input: "bbbbb"
//Output: 1
//Explanation: The answer is "b", with the length of 1.
//Example 3:
//
//Input: "pwwkew"
//Output: 3
//Explanation: The answer is "wke", with the length of 3.
//Note that the answer must be a substring, "pwke" is a subsequence and not a substring.

object LongestSubstringWithoutRepeatingCharacters extends App {

  def lengthOfLongestSubstring(s: String): Int = {
    s
      .toList
      .foldLeft((List.empty[Char], 0, 0)) { (currentState, nextChar) =>
        val (currentString, currentLength, maxLength) = currentState
        if (currentString.contains(nextChar))
            {
              val nextString = currentString.dropWhile(_ != nextChar).drop(1) :+ nextChar
              (nextString, nextString.length, maxLength)
            }
        else
          (currentString :+ nextChar, currentLength + 1, Math.max(currentLength + 1, maxLength))
    }._3

  }

  println(s"longest unique substring of abcda is ${lengthOfLongestSubstring("abcda")}")
  println(s"longest unique substring of abcabcbb is ${lengthOfLongestSubstring("abcabcbb")}")
  println(s"longest unique substring of aabaab!bb is ${lengthOfLongestSubstring("aabaab!bb")}")
  println(s"longest unique substring of aab is ${lengthOfLongestSubstring("aab")}")

}
