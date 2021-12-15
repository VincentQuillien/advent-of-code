import scala.io.Source

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

val distanceMap =
  grid.view.mapValues(_ => Int.MaxValue - 9).toMap.updated((0, 0), 0)

def dijkstra(
    point: (Int, Int) = (0, 0),
    distanceMap: Map[(Int, Int), Int] = distanceMap
): Int = {
  if (point == (sideLength - 1, sideLength - 1)) distanceMap(point)
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
      updatedDistanceMap
    )

  }
}

val part1 = dijkstra()
