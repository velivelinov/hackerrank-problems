package leetcode.thirtydaysofcode.weekone

//Given an array of strings, group anagrams together.
//
//Example:
//
//Input: ["eat", "tea", "tan", "ate", "nat", "bat"],
//Output:
//[
//["ate","eat","tea"],
//["nat","tan"],
//["bat"]
//]
//Note:
//
//All inputs will be in lowercase.
//The order of your output does not matter.

object GroupAnagrams {

  def groupAnagrams(strs: Array[String]): List[List[String]] = {

    def encode(str: String): Set[(Char, Int)] =
      str.groupBy(identity).mapValues(_.length).toSet

    strs.toList
      .map(str => (encode(str) -> str))
      .groupBy(_._1)
      .mapValues(_.map(_._2))
      .values
      .toList
  }
}
