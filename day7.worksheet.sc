import scala.io.Source
val input =
  Source.fromFile("day7.input.txt").getLines.next.split(',').map(_.toInt).toList

// Part1
(input.min to input.max)
  .map(i => input.map(crabPos => (crabPos - i).abs).sum)
  .min

// Part2
def sumOfAllIntToN(n: Int) = n * (n - 1) / 2 + n

(input.min to input.max)
  .map(i => input.map(crabPos => sumOfAllIntToN((crabPos - i).abs)).sum)
  .min
