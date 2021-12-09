import scala.io.Source

val input = Source
  .fromFile("day8.input.txt")
  .getLines
  .map { line =>
    val left :: right :: Nil = line.split(" \\| ").toList
    (left.split(' ').toList, right.split(' ').toList)
  }
  .toList

// Part 1
input.map { case (_, right) =>
  right.count(List(2, 3, 4, 7) contains _.length)
}.sum

// Part 2
def part2(lines: List[(List[String], List[String])] = input): Int =
  lines match {
    case Nil => 0
    case (left, right) :: tail => {
      val (knowns, unknowns) = (left ++ right)
        .map(_.toCharArray.toSet)
        .toSet
        .partition(_.size < 5)

      val one :: seven :: four :: Nil = knowns.toList.sortBy(_.size)
      val nine = unknowns
        .find(unknown =>
          unknown.size == 6 && unknown.diff(one ++ seven ++ four).size == 1
        )
        .get

      val two = unknowns
        .find(unknown => unknown.size == 5 && nine.diff(unknown).size == 2)
        .get

      val six = unknowns
        .find(unknown =>
          unknown.size == 6 && seven.intersect(unknown).size == 2
        )
        .get
      val five = unknowns
        .find(unknown => unknown.size == 5 && six.diff(unknown).size == 1)
        .get

      val three = unknowns
        .find(unknown => unknown.size == 5 && unknown != two && unknown != five)
        .get

      val eight = unknowns
        .find(unknown => unknown.size == 7)
        .get

      val zero = unknowns
        .find(unknown => unknown.size == 6 && unknown != six && unknown != nine)
        .get

      val segments =
        Vector(zero, one, two, three, four, five, six, seven, eight, nine)

      right
        .map(_.toCharArray.toSet)
        .map(segments.indexOf(_))
        .mkString
        .toInt + part2(tail)
    }
  }

part2()
