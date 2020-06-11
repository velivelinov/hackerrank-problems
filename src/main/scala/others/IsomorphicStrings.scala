package others

//Given two strings s and t, determine if they are isomorphic.
//
//Two strings are isomorphic if the characters in s can be replaced to get t.
//
//All occurrences of a character must be replaced with another character while preserving the order of characters.
//No two characters may map to the same character but a character may map to itself.
//
//Example 1:
//
//Input: s = "egg", t = "add"
//Output: true
//Example 2:
//
//Input: s = "foo", t = "bar"
//Output: false
//Example 3:
//
//Input: s = "paper", t = "title"
//Output: true

object IsomorphicStrings extends App {

  def isIsomorphic(s: String, t: String): Boolean = {
    s
      .zipAll(t, " ".charAt(0) , " ".charAt(0) )
      .foldLeft(Map.empty[Char, Char]) { (soFar, next) =>
        val (sChar, tChar) = next

        soFar.get(sChar) match {
          case Some(mapping) if mapping==tChar => soFar
          case Some(_) => return false
          case None => soFar + ((sChar, tChar))
        }
      }

    true
  }

  println(isIsomorphic("aa", "ab"))
}
