import scala.io.Source

val input = Source
  .fromFile("day11.input.txt")
  .getLines

type Grid = Map[(Int, Int), Int]

val grid = {
  for (
    (line, y) <- input.zipWithIndex;
    (energy, x) <- line.zipWithIndex
  )
    yield ((x, y) -> energy.asDigit)
}.toMap

def getAdjacents(px: Int, py: Int) =
  for (
    x <- px - 1 to px + 1;
    y <- py - 1 to py + 1
    if x != px || y != py
  )
    yield (x, y)

def propagateFlash(
    grid: Grid,
    point: (Int, Int)
): Grid =
  grid.get(point) match {
    case None                       => grid
    case Some(energy) if energy < 9 => grid.updated(point, grid(point) + 1)
    case Some(energy) if energy == 9 => {
      val updatedGrid = grid.updated(point, 10)
      val adjacents = getAdjacents.tupled(point)
      adjacents.foldLeft(updatedGrid) { propagateFlash(_, _) }
    }
    case _ => grid
  }

def part1(grid: Grid, steps: Int): Int = {
  val stepFlashes = grid.values.count(_ == 0)
  if (steps == 0) stepFlashes
  else {
    val newGrid = grid.keys
      .foldLeft(grid)(propagateFlash(_, _))
      .view
      .mapValues(energy => if (energy == 10) 0 else energy)
      .toMap
    stepFlashes + part1(newGrid, steps - 1)
  }
}
part1(grid, steps = 100)

def part2(grid: Grid): Int = {
  if (grid.values.forall(_ == 0)) 0
  else {
    val newGrid = grid.keys
      .foldLeft(grid)(propagateFlash(_, _))
      .view
      .mapValues(energy => if (energy == 10) 0 else energy)
      .toMap
    1 + part2(newGrid)
  }
}

part2(grid)
