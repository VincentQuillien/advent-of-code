import scala.io.Source
val input = Source.fromFile("day16.input.txt").getLines.next
val bits =
  input
    .map(c => leftPad(BigInt(c.toString, 16).toString(2)))
    .mkString

enum Packet:
    val version: Int
    case Literal(version: Int, value: BigInt) extends Packet
    case Operator(version: Int, typeID: Int, operands: List[Packet]) extends Packet

def parse(bits: String): (String, Packet) = {
  val version = BigInt(bits.slice(0, 3), 2).toInt
  val typeID = BigInt(bits.slice(3, 6), 2).toInt
  val packetContent = bits.drop(6)
  if (typeID == 4) {
    val (rest, bitGroups) = parseLiteral(packetContent)
    (rest, Packet.Literal(version = version, value = BigInt(bitGroups, 2)))
  } else {
    val lengthTypeId = packetContent.head
    if (lengthTypeId == '0') {
      val subPacketsBitsLeft = BigInt(packetContent.tail.slice(0, 15), 2).toInt
      val (rest, packets) =
        parseOperandsMode0(subPacketsBitsLeft, packetContent.tail.drop(15))
      (rest, Packet.Operator(version, typeID, operands = packets))
    } else {
      val subPacketsLeft = BigInt(packetContent.tail.slice(0, 11), 2).toInt
      val (rest, packets) =
        parseOperandsMode1(subPacketsLeft, packetContent.tail.drop(11))
      (rest, Packet.Operator(version, typeID, operands = packets))
    }
  }
}

def parseLiteral(bits: String): (String, String) = {
  val prefix = bits.head
  val bitGroup = bits.slice(1, 5)
  if (prefix == '0') (bits.drop(5), bitGroup)
  else {
    val (rest, bitGroups) = parseLiteral(bits.drop(5))
    (rest, bitGroup + bitGroups)
  }
}

def parseOperandsMode0(bitsLeft: Int, bits: String): (String, List[Packet]) = {
  if (bitsLeft == 0) (bits, List.empty)
  else {
    val (rest, packet) = parse(bits)
    val (rest2, packets) =
      parseOperandsMode0(bitsLeft - (bits.length - rest.length), rest)
    (rest2, packet :: packets)
  }
}

def parseOperandsMode1(
    packetsLeft: Int,
    bits: String
): (String, List[Packet]) = {
  if (packetsLeft == 0) (bits, List.empty)
  else {
    val (rest, packet) = parse(bits)
    val (rest2, packets) = parseOperandsMode1(packetsLeft - 1, rest)
    (rest2, packet :: packets)
  }
}

def foldPackets[T](packet: Packet, initialValue: T)(f: (T, Packet) => T): T = {
  val acc = f(initialValue, packet)
  packet match {
    case Packet.Literal(_, _) => acc
    case Packet.Operator(_, _, operands) =>
      operands.foldLeft(acc) { (acc, packet) => foldPackets(packet, acc)(f) }
  }
}

def leftPad(s: String) = s.reverse.padTo(4, '0').reverse

val (_, packet) = parse(bits)
val part1 = foldPackets(packet, 0) { (acc, packet) => acc + packet.version }

val operatorActions = Vector[List[BigInt] => BigInt](
  _.sum,
  _.product,
  _.min,
  _.max,
  { _ => 0 },
  { case fst :: snd :: _ => if (fst > snd) 1 else 0 },
  { case fst :: snd :: _ => if (fst < snd) 1 else 0 },
  { case fst :: snd :: _ => if (fst == snd) 1 else 0 }
)

def evaluatePacket(packet: Packet): BigInt = {
  packet match {
    case Packet.Literal(_, value) => value
    case Packet.Operator(_, typeID, operands) =>
      operatorActions(typeID)(operands.map(evaluatePacket(_)))
  }
}

val part2 = evaluatePacket(packet)
