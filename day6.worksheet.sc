import scala.io.Source

val input = Source.fromFile("day6.input.txt").getLines.next.split(',').map(_.toInt).toVector

val fishByStage = input.foldLeft(Vector.fill(9){0L}) { (acc, cv) => acc.updated(cv, acc(cv) + 1) }

def getPopulation(daysLeft: Int, fishByStage: Vector[Long] = fishByStage): Long = 
    if daysLeft == 0 then fishByStage.sum else {
        val head +: tail = fishByStage
        getPopulation(daysLeft - 1, tail.updated(6, tail(6) + head).appended(head))
    }

//Part 1
getPopulation(80)

// Part2
getPopulation(256)
