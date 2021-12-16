import scala.io.Source
import annotation.tailrec

val input = Source
  .fromFile("day15.input.txt")
  .getLines
  .toList

val sideLength = input.head.length

type Grid = Map[(Int, Int), Int]

val grid = {
  for (
    (line, y) <- input.zipWithIndex;
    (risk, x) <- line.zipWithIndex
  )
    yield ((x, y) -> risk.asDigit)
}.toMap

def getAdjacents(x: Int, y: Int) =
  List((x, y + 1), (x + 1, y), (x, y - 1), (x - 1, y))

def initDistanceMap(grid: Grid) =
  grid.view.mapValues(_ => Int.MaxValue - 9).toMap.updated((0, 0), 0)

@tailrec final def dijkstra(
    point: (Int, Int) = (0, 0),
    distanceMap: Map[(Int, Int), Int],
    grid: Grid
): Int = {
  val end = Math.sqrt(grid.size).toInt - 1
  if (point == (end, end))
    distanceMap(point)
  else {
    val adjacents = getAdjacents.tupled(point).filter(distanceMap.contains(_))
    val cumulatedRisk = distanceMap(point)
    val updatedDistanceMap = adjacents
      .foldLeft(distanceMap.removed(point)) { (distanceMap, adjacentPoint) =>
        val risk = grid(adjacentPoint)
        distanceMap.updated(
          adjacentPoint,
          distanceMap(adjacentPoint).min(
            cumulatedRisk + risk
          )
        )
      }
    val (minPoint, p) =
      updatedDistanceMap.minBy((point, cumulatedRisk) => cumulatedRisk)
    dijkstra(
      minPoint,
      updatedDistanceMap,
      grid
    )
  }
}

val part1 = dijkstra(
  distanceMap = initDistanceMap(grid),
  grid = grid
)

val gridTimes5 = {
  for (y <- 0 until sideLength * 5; x <- 0 until sideLength * 5) yield {
    val order = x / sideLength + y / sideLength
    val risk = (grid(x % sideLength, y % sideLength) + order)
    ((x, y) -> (if (risk > 9) risk - 9 else risk))
  }
}.toMap

dijkstra(distanceMap = initDistanceMap(gridTimes5), grid = gridTimes5)
