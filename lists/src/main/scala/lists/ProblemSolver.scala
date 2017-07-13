package lists

/**
	Problem described here: http://projecteuler.net/problem=67
*/
object ProblemSolver {

  type Path = List[Int]
  type Paths = List[Path]

  // find optimal solution (path with maximum sum)
  // for every number from the *bottom* line of the triangle 
  def findOptimalPaths(data: Paths): Paths = {

    def squash(bottom: Paths, rest: Paths): Paths = rest match {
      case Nil => bottom
      case head :: tail =>
        val merged: Paths = merge(bottom, head)
        squash(merged, tail)
    }

    def merge(bottom: Paths, rest: Path): Paths =
      List.range(0, bottom.indices.size - 1).map(i => rest(i) :: maxPath(i, bottom))

    def maxPath(i:Int, bottom: Paths): Path =
      if (bottom(i).sum > bottom(i + 1).sum) bottom(i)
      else bottom(i + 1)


    val reversed = data.reverse
    squash(reversed.head.map(List(_)), reversed.tail)
  }

  // choose best path that have maximum sum
  def chooseBestPath(paths: Paths): Option[Path] = paths match {
    case Nil => None
    case list => list.find(_.sum == list.map(_.sum).max)
  }
}