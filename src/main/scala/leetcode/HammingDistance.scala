package leetcode

//The Hamming distance between two integers is the number of positions at which the corresponding bits are different.
//
//Given two integers x and y, calculate the Hamming distance.
//
//Note:
//0 ≤ x, y < 231.
//
//Example:
//
//Input: x = 1, y = 4
//
//Output: 2
//
//Explanation:
//1   (0 0 0 1)
//4   (0 1 0 0)
//↑   ↑
//
//The above arrows point to positions where the corresponding bits are different.
object HammingDistance extends App {

  def hammingDistance(x: Int, y: Int): Int = {
    val binaryX = x.toBinaryString
    val binaryY = y.toBinaryString

    val (longer, shorter) = if (binaryX.length > binaryY.length) (binaryX, binaryY) else (binaryY, binaryX)

    s"${"0" * (longer.length - shorter.length)}$shorter"
      .zip(longer)
      .count{ case (left, right) => left != right }
  }

  println(hammingDistance(4, 14))
  println(hammingDistance(1, 4))
}
