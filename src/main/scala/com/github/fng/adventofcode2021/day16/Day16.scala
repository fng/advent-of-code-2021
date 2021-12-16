package com.github.fng.adventofcode2021.day16

object Day16 {

  sealed trait Package {
    val version: Long
    def versionSum: Long = version
  }

  case class LiteralPackage(
      version: Long,
      packageType: Long,
      literalBinary: String,
      literalDecimal: Long
  ) extends Package

  case class Operator(
      version: Long,
      packageType: Long,
      lengthTypeId: Long,
      packages: List[Package]
  ) extends Package {
    override def versionSum: Long =
      this.version + packages.map(_.versionSum).sum
  }

  def parseHex(hex: String): (Package, String) = parse(hexStringToBinary(hex))

  def parse(binary: String): (Package, String) = {
    val (versionBinary, withoutVersion) = binary.splitAt(3)
    val (typeIdBinary, withoutTypeId) = withoutVersion.splitAt(3)

    val version = binaryToLong(versionBinary)
    val typeId = binaryToLong(typeIdBinary)

    if (typeId == 4) {
      //Literal
      val literalGroups = withoutTypeId.grouped(5).toList
      val lastLiteralGroup = literalGroups.indexWhere(_.startsWith("0"))
      val literalBinary = literalGroups
        .take(lastLiteralGroup + 1)
        .map { literalGroup =>
          //drop first bit
          literalGroup.tail
        }
        .mkString

      val rest = literalGroups.drop(lastLiteralGroup + 1).mkString
      (
        LiteralPackage(
          version,
          typeId,
          literalBinary,
          binaryToLong(literalBinary)
        ),
        rest
      )
    } else {
      //Operator
      val (lengthTypeId, withoutLengthType) = withoutTypeId.splitAt(1)

      if (lengthTypeId == "0") {
        val (subPackagesLengthBinary, withoutSubPackagesLength) =
          withoutLengthType.splitAt(15)
        val subPackageLength = binaryToLong(subPackagesLengthBinary)
        val (subPackagesBits, rest) =
          withoutSubPackagesLength.splitAt(subPackageLength.toInt)

        def parseRec(bits: String): List[Package] = {
          val (parsedPackage, rest) = parse(bits)
          if (rest.length > 6) List(parsedPackage) ++ parseRec(rest)
          else List(parsedPackage)
        }

        val subPackages = parseRec(subPackagesBits)
        (Operator(version, typeId, lengthTypeId.toInt, subPackages), rest)

      } else {
        val (numberOfSubPackagesLengthBinary, withoutSubPackagesLength) =
          withoutLengthType.splitAt(11)
        val numberOfSubPackages = binaryToLong(numberOfSubPackagesLengthBinary)

        def parseNPackages(
            bits: String,
            alreadyParsed: List[Package],
            remainingPackages: Int
        ): (List[Package], String) = {
          if (remainingPackages == 0) (alreadyParsed, bits)
          else {
            val (parsePackage, rest) = parse(bits)
            parseNPackages(
              rest,
              alreadyParsed :+ parsePackage,
              remainingPackages - 1
            )
          }
        }

        val (subPackages, rest) = parseNPackages(
          withoutSubPackagesLength,
          Nil,
          numberOfSubPackages.toInt
        )

        if (subPackages.length > numberOfSubPackages)
          sys.error("More sub packages found than expected")

        (Operator(version, typeId, lengthTypeId.toInt, subPackages), rest)
      }

    }

  }

  def binaryToLong(binary: String): Long = {
    java.lang.Long.parseLong(binary, 2)
  }

  def hexStringToBinary(hex: String): String = {
    hex.toCharArray
      .map(_.toString)
      .toList
      .map {
        case "0" => "0000"
        case "1" => "0001"
        case "2" => "0010"
        case "3" => "0011"
        case "4" => "0100"
        case "5" => "0101"
        case "6" => "0110"
        case "7" => "0111"
        case "8" => "1000"
        case "9" => "1001"
        case "A" => "1010"
        case "B" => "1011"
        case "C" => "1100"
        case "D" => "1101"
        case "E" => "1110"
        case "F" => "1111"
      }
      .mkString
  }

}
