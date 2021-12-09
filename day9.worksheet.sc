import scala.io.Source

val grid = Source
  .fromFile("day9.input.txt")
  .getLines
  .zipWithIndex
  .map { case (line, lineIndex) =>
    line.toCharArray.zipWithIndex.foldLeft(Map[(Int, Int), Int]()) {
      case (acc, (char, columnIndex)) =>
        acc + ((columnIndex, lineIndex) -> char.asDigit)
    }
  }
  .reduce(_ ++ _)

// Part 1
val up = ((x: Int, y: Int) => (x, y - 1)).tupled
val right = ((x: Int, y: Int) => (x + 1, y)).tupled
val down = ((x: Int, y: Int) => (x, y + 1)).tupled
val left = ((x: Int, y: Int) => (x - 1, y)).tupled

val lowPoints = grid
  .filter { (point, floor) =>
    List(
      grid.getOrElse(up(point), 10),
      grid.getOrElse(right(point), 10),
      grid.getOrElse(down(point), 10),
      grid.getOrElse(left(point), 10)
    ).min > floor
  }

val part1 = lowPoints.values
  .map(_ + 1)
  .sum

// Part 2
// ChartMap = Key: point, Value: isValid (in-bounds and not already visited)
type ChartMap = Map[(Int, Int), Boolean]

def floodFill(
    point: (Int, Int),
    chartMap: ChartMap = grid.view.mapValues { _ != 9 }.toMap
): (Int, ChartMap) = {
  if !chartMap.getOrElse(point, false) then (0, chartMap) else {
    val (surfaceUp, chartMapUp) = floodFill(up(point), chartMap.updated(point, false))
    val (surfaceRight, chartMapRight) = floodFill(right(point), chartMapUp)
    val (surfaceDown, chartMapDown) = floodFill(down(point), chartMapRight)
    val (surfaceLeft, chartMapLeft) = floodFill(left(point), chartMapDown)
    (1 + surfaceUp + surfaceRight + surfaceDown + surfaceLeft, chartMapLeft)
  }
}

val part2 = lowPoints.keys.toList.map(floodFill(_)).map((n, _) => n).sorted.takeRight(3).product