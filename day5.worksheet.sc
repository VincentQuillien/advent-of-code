import scala.io.Source
val input = Source
  .fromFile("day5.input.txt")
  .getLines
  .map("([0-9]+)".r.findAllMatchIn(_).toList.map(_.toString.toInt))
  .map { case x1 :: y1 :: x2 :: y2 :: _ => ((x1, y1), (x2, y2)) }
  .toList

def getAxisValues(a1: Int, a2: Int, lineLength: Int) = a1 - a2 match
    case 0 => List.fill(lineLength){a1}
    case x if x > 0 => (a1 to a2 by -1).toList
    case x if x < 0 => (a1 to a2).toList

def getHotPoints(lines: List[((Int, Int), (Int, Int))]): Map[(Int, Int), Int] = lines match 
    case Nil => Map()
    case ((x1, y1), (x2, y2))::tail => {
        val hotPoints = getHotPoints(tail)
        val lineLength = Math.max(Math.abs(x1 - x2), Math.abs(y1 - y2)) + 1
        val xs = getAxisValues(x1, x2, lineLength)
        val ys = getAxisValues(y1, y2, lineLength)
        (xs zip ys).foldLeft(hotPoints) { (acc, point) =>
            val occurence = acc.getOrElse(point, 0) + 1
            acc.updated(point, occurence)
        }
    }

val straightLines = input.filter { case ((x1, y1), (x2, y2)) => x1 == x2 || y1 == y2 }
val diagonalLines = input.filter { case ((x1, y1), (x2, y2)) => Math.abs(x1 - x2) == Math.abs(y1 - y2) }

// Part 1
getHotPoints(straightLines).count((_, occurence) => occurence > 1)

// Part2
getHotPoints(straightLines ++ diagonalLines).count((_, occurence) => occurence > 1)