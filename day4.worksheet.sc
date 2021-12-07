import scala.io.Source

// Parsing
val (numbers, gridSets) = {
    val numbersLine :: separator :: gridLines = Source.fromFile("day4.input.txt").getLines.toList

    val numbers = numbersLine.split(",").map(_.toInt).toList

    def parseGridLines(lines: List[String] = gridLines): List[List[List[Int]]] = 
        lines match
            case Nil => List(List())
            case ""::tail => Nil :: parseGridLines(tail)
            case line::tail => {
                val grid :: grids = parseGridLines(tail)
                (line.trim().split("\\s+").map(_.toInt).toList :: grid)::grids
            }

    val grids = parseGridLines()

    val gridSets = grids.map(grid => {
        val lineSets = grid.map(_.toSet).toVector
        val columnSets = (0 until 5).map(i => grid.map(line => line(i)).toSet).toVector
        lineSets ++ columnSets
    }).toSet

    (numbers, gridSets)
}

def part1(numbers: List[Int] = numbers, drawnNumbers: Set[Int] = Set[Int]()): Int = 
    val n::tail = numbers
    val updatedDrawnNumbers = drawnNumbers + n
    val winningGrid = gridSets.find(_.exists(_.intersect(updatedDrawnNumbers).size == 5))
    winningGrid match
        case None => part1(tail, updatedDrawnNumbers)
        case Some(winningGrid) => winningGrid.reduce(_ ++ _).diff(updatedDrawnNumbers).sum * n

part1()

def part2(numbers: List[Int] = numbers, drawnNumbers: Set[Int] = Set[Int](), grids: Set[Vector[Set[Int]]] = gridSets): Int = 
    val n::tail = numbers
    val updatedDrawnNumbers = drawnNumbers + n
    val winningGrids = grids.filter(_.exists(_.intersect(updatedDrawnNumbers).size == 5))
    val remainingGrids = grids.diff(winningGrids)
    if remainingGrids.isEmpty 
        then winningGrids.head.reduce(_ ++ _).diff(updatedDrawnNumbers).sum * n 
        else part2(tail, updatedDrawnNumbers, remainingGrids)


part2()
