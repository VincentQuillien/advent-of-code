import scala.io.Source
val input = Source
  .fromFile("day12.input.txt")
  .getLines
  .map(_.split('-').take(2).toList)
  .toList

val connectedCaves =
  input.foldLeft(Map[String, List[String]]()) { case (acc, p1 :: p2 :: Nil) =>
    acc
      .updated(p1, p2 :: acc.getOrElse(p1, Nil))
      .updated(p2, p1 :: acc.getOrElse(p2, Nil))
  }

def isSmallCave(cave: String) = cave.matches("([a-z].*)")

def getPaths(
    cave: String = "start",
    visitedCaves: Set[String] = Set(),
    smallCaveJoker: Int = 0
): Vector[Set[String]] = cave match {
  case "end" => Vector(visitedCaves)
  case cave
      if visitedCaves.contains(
        cave
      ) && (smallCaveJoker == 0 || cave == "start") =>
    Vector.empty
  case cave if visitedCaves.contains(cave) =>
    getPaths(cave, visitedCaves - cave, smallCaveJoker - 1)
  case cave => {
    connectedCaves(cave).foldLeft(Vector[Set[String]]()) { (acc, cv) =>
      acc ++ getPaths(
        cv,
        if (cave.matches("([a-z].*)")) visitedCaves + cave else visitedCaves,
        smallCaveJoker
      )
    }
  }
}

val part1 = getPaths().size

val part2 = getPaths(smallCaveJoker = 1).size
