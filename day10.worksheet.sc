import scala.io.Source

val input = Source.fromFile("day10.input.txt").getLines.map(_.toCharArray.toList).toList

val delimitersMapping = Map('(' -> ')', '[' -> ']', '<' -> '>', '{' -> '}')
val openingDelimiters = delimitersMapping.keySet

def getInvalidBraceOrStack(line: List[Char], stack: List[Char] = List()): Char | List[Char] = line match {
    case Nil => stack
    case delimiter::tail if openingDelimiters.contains(delimiter) => 
        getInvalidBraceOrStack(tail, delimiter::stack)
    case delimiter::tail => 
        if delimitersMapping(stack.head) == delimiter
            then getInvalidBraceOrStack(tail, stack.tail)
            else delimiter
}

val part1 = input.map(getInvalidBraceOrStack(_)).collect { case delimiter: Char => delimiter }.map {
        case ')' => 3
        case ']' => 57
        case '}' => 1197
        case '>' => 25137
    }.sum


val part2List = input.map(getInvalidBraceOrStack(_)).collect { case stack: List[Char] => stack }.map { stack =>
        val closingDelimiters = stack.map(delimitersMapping(_))
        closingDelimiters.map {
            case ')' => 1
            case ']' => 2
            case '}' => 3
            case '>' => 4
        }.foldLeft(0L) {(acc, cv) => 5 * acc + cv}
    }.sorted

val part2 = part2List(part2List.length / 2)