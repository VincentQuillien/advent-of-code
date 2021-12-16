import scala.io.Source
val input = Source.fromFile("day16.input.txt").getLines.next

def leftPad(s: String) = s.reverse.padTo(4, '0').reverse

def parsePacket(bits: String): (Int, String) = {
  val version = BigInt(bits.slice(0, 3), 2).toInt
  val typeId = BigInt(bits.slice(3, 6), 2)
  val packetContent = bits.drop(6)
  if (typeId == 4) {
    (version, literalValues(packetContent))
  } else {
    val lengthTypeId = packetContent.head
    if (lengthTypeId == '0') {
      val subPacketsBitsLeft = BigInt(packetContent.tail.slice(0, 15), 2).toInt
      val (versionSum, rest) =
        subPacketsMode0(subPacketsBitsLeft, packetContent.tail.drop(15))
      (versionSum + version, rest)
    } else {
      val subPacketsLeft = BigInt(packetContent.tail.slice(0, 11), 2).toInt
      val (versionSum, rest) =
        subPacketsMode1(subPacketsLeft, packetContent.tail.drop(11))
      (versionSum + version, rest)
    }
  }
}

def subPacketsMode0(bitsLeft: Int, bits: String): (Int, String) = {
  if (bitsLeft == 0) (0, bits)
  else {
    val (version, rest) = parsePacket(bits)
    val (versionSum, rest2) =
      subPacketsMode0(bitsLeft - (bits.length - rest.length), rest)
    (versionSum + version, rest2)
  }
}

def subPacketsMode1(packetsLeft: Int, bits: String): (Int, String) = {
  if (packetsLeft == 0) (0, bits)
  else {
    val (version, rest) = parsePacket(bits)
    val (versionSum, rest2) = subPacketsMode1(packetsLeft - 1, rest)
    (versionSum + version, rest2)
  }
}

def literalValues(bits: String): String =
  if (bits(0) == '0') bits.drop(5) else literalValues(bits.drop(5))

val bits =
  input
    .map(c => leftPad(BigInt(c.toString, 16).toString(2)))
    .mkString

val (part1, _) = parsePacket(bits)
