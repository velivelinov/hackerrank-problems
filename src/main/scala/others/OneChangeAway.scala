package others

object OneChangeAway extends App {

  def isOneChangeAway(first: String, second: String): Boolean = {

    //baker baked
    //if same => aker + aked
    //if different and haven't had diff => switch boolean
    //if different and have had diff => return false

    //baker + bake
    def helper(first: List[Char], second: List[Char], hasEncounteredDiff: Boolean): Boolean = {
      (first, second) match {
        case (headOne :: tailOne, headTwo :: tailTwo) if headOne == headTwo =>
          helper(tailOne, tailTwo, hasEncounteredDiff)
        case (_ :: tailOne, _ :: tailTwo) if !hasEncounteredDiff =>
          helper(tailOne, tailTwo, true)
        case (_ :: tailOne, _ :: tailTwo) if hasEncounteredDiff => false
        case (headOne :: Nil, Nil) if !hasEncounteredDiff => true
        case (headOne :: Nil, Nil) => false
        case (Nil, headTwo :: Nil) if !hasEncounteredDiff => true
        case (Nil, headTwo :: Nil) => false
        case (Nil, Nil) => true
      }
    }

    if (Math.abs(first.length - second.length) > 1) false
    else helper(first.toList, second.toList, false)

  }

  println(s"baker, bake ${isOneChangeAway("baker", "bake")}")
  println(s"baker, baked ${isOneChangeAway("baker", "baked")}")
  println(s"baking, baked ${isOneChangeAway("baking", "baked")}")
}
